------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P_ D I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;       use Atree;
with Einfo;       use Einfo;
with Elists;      use Elists;
with Exp_Strm;    use Exp_Strm;
with Exp_Tss;     use Exp_Tss;
with Exp_Util;    use Exp_Util;
with GNAT.HTable; use GNAT.HTable;
with Lib;         use Lib;
with Namet;       use Namet;
with Nlists;      use Nlists;
with Nmake;       use Nmake;
with Opt;         use Opt;
with Rtsfind;     use Rtsfind;
with Sem;         use Sem;
with Sem_Ch3;     use Sem_Ch3;
with Sem_Ch8;     use Sem_Ch8;
with Sem_Dist;    use Sem_Dist;
with Sem_Eval;    use Sem_Eval;
with Sem_Util;    use Sem_Util;
with Sinfo;       use Sinfo;
with Snames;      use Snames;
with Stand;       use Stand;
with Stringt;     use Stringt;
with Tbuild;      use Tbuild;
with Ttypes;      use Ttypes;
with Uintp;       use Uintp;

package body Exp_Dist is

   --  The following model has been used to implement distributed objects:
   --  given a designated type D and a RACW type R, then a record of the
   --  form:

   --    type Stub is tagged record
   --       [...declaration similar to s-parint.ads RACW_Stub_Type...]
   --    end record;

   --  is built. This type has two properties:

   --    1) Since it has the same structure than RACW_Stub_Type, it can be
   --       converted to and from this type to make it suitable for
   --       System.Partition_Interface.Get_Unique_Remote_Pointer in order
   --       to avoid memory leaks when the same remote object arrive on the
   --       same partition through several paths;

   --    2) It also has the same dispatching table as the designated type D,
   --       and thus can be used as an object designated by a value of type
   --       R on any partition other than the one on which the object has
   --       been created, since only dispatching calls will be performed and
   --       the fields themselves will not be used. We call Derive_Subprograms
   --       to fake half a derivation to ensure that the subprograms do have
   --       the same dispatching table.

   First_RCI_Subprogram_Id : constant := 2;
   --  RCI subprograms are numbered starting at 2. The RCI receiver for
   --  an RCI package can thus identify calls received through remote
   --  access-to-subprogram dereferences by the fact that they have a
   --  (primitive) subprogram id of 0, and 1 is used for the internal
   --  RAS information lookup operation. (This is for the Garlic code
   --  generation, where subprograms are identified by numbers; in the
   --  PolyORB version, they are identified by name, with a numeric suffix
   --  for homonyms.)

   type Hash_Index is range 0 .. 50;

   -----------------------
   -- Local subprograms --
   -----------------------

   function Hash (F : Entity_Id) return Hash_Index;
   --  DSA expansion associates stubs to distributed object types using
   --  a hash table on entity ids.

   function Hash (F : Name_Id)   return Hash_Index;
   --  The generation of subprogram identifiers requires an overload counter
   --  to be associated with each remote subprogram names. These counters
   --  are maintained in a hash table on name ids.

   type Subprogram_Identifiers is record
      Str_Identifier : String_Id;
      Int_Identifier : Int;
   end record;

   package Subprogram_Identifier_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Subprogram_Identifiers,
                         No_Element => (No_String, 0),
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a remote subprogram and the corresponding
   --  subprogram identifiers.

   package Overload_Counter_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Int,
                         No_Element => 0,
                         Key        => Name_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a subprogram name and an integer that
   --  counts the number of defining subprogram names with that
   --  Name_Id encountered so far in a given context (an interface).

   function Get_Subprogram_Ids (Def : Entity_Id) return Subprogram_Identifiers;
   function Get_Subprogram_Id  (Def : Entity_Id) return String_Id;
   function Get_Subprogram_Id  (Def : Entity_Id) return Int;
   --  Given a subprogram defined in a RCI package, get its distribution
   --  subprogram identifiers (the distribution identifiers are a unique
   --  subprogram number, and the non-qualified subprogram name, in the
   --  casing used for the subprogram declaration; if the name is overloaded,
   --  a double underscore and a serial number are appended.
   --
   --  The integer identifier is used to perform remote calls with GARLIC;
   --  the string identifier is used in the case of PolyORB.
   --
   --  Although the PolyORB DSA receiving stubs will make a caseless comparison
   --  when receiving a call, the calling stubs will create requests with the
   --  exact casing of the defining unit name of the called subprogram, so as
   --  to allow calls to subprograms on distributed nodes that do distinguish
   --  between casings.
   --
   --  NOTE: Another design would be to allow a representation clause on
   --  subprogram specs: for Subp'Distribution_Identifier use "fooBar";

   pragma Warnings (Off, Get_Subprogram_Id);
   --  One homonym only is unreferenced (specific to the GARLIC version)

   procedure Add_RAS_Dereference_TSS (N : Node_Id);
   --  Add a subprogram body for RAS Dereference TSS

   procedure Add_RAS_Proxy_And_Analyze
     (Decls              : List_Id;
      Vis_Decl           : Node_Id;
      All_Calls_Remote_E : Entity_Id;
      Proxy_Object_Addr  : out Entity_Id);
   --  Add the proxy type necessary to call the subprogram declared
   --  by Vis_Decl through a remote access to subprogram type.
   --  All_Calls_Remote_E must be Standard_True if a pragma All_Calls_Remote
   --  applies, Standard_False otherwise. The new proxy type is appended
   --  to Decls. Proxy_Object_Addr is a constant of type System.Address that
   --  designates an instance of the proxy object.

   function Build_Remote_Subprogram_Proxy_Type
     (Loc            : Source_Ptr;
      ACR_Expression : Node_Id) return Node_Id;
   --  Build and return a tagged record type definition for an RCI
   --  subprogram proxy type.
   --  ACR_Expression is use as the initialization value for
   --  the All_Calls_Remote component.

   function Build_Get_Unique_RP_Call
     (Loc       : Source_Ptr;
      Pointer   : Entity_Id;
      Stub_Type : Entity_Id) return List_Id;
   --  Build a call to Get_Unique_Remote_Pointer (Pointer), followed by a
   --  tag fixup (Get_Unique_Remote_Pointer may have changed Pointer'Tag to
   --  RACW_Stub_Type'Tag, while the desired tag is that of Stub_Type).

   function Build_Subprogram_Calling_Stubs
     (Vis_Decl                 : Node_Id;
      Subp_Id                  : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Locator                  : Entity_Id := Empty;
      New_Name                 : Name_Id   := No_Name) return Node_Id;
   --  Build the calling stub for a given subprogram with the subprogram ID
   --  being Subp_Id. If Stub_Type is given, then the "addr" field of
   --  parameters of this type will be marshalled instead of the object
   --  itself. It will then be converted into Stub_Type before performing
   --  the real call. If Dynamically_Asynchronous is True, then it will be
   --  computed at run time whether the call is asynchronous or not.
   --  Otherwise, the value of the formal Asynchronous will be used.
   --  If Locator is not Empty, it will be used instead of RCI_Cache. If
   --  New_Name is given, then it will be used instead of the original name.

   function Build_RPC_Receiver_Specification
     (RPC_Receiver      : Entity_Id;
      Request_Parameter : Entity_Id) return Node_Id;
   --  Make a subprogram specification for an RPC receiver, with the given
   --  defining unit name and formal parameter.

   function Build_Ordered_Parameters_List (Spec : Node_Id) return List_Id;
   --  Return an ordered parameter list: unconstrained parameters are put
   --  at the beginning of the list and constrained ones are put after. If
   --  there are no parameters, an empty list is returned. Special case:
   --  the controlling formal of the equivalent RACW operation for a RAS
   --  type is always left in first position.

   procedure Add_Calling_Stubs_To_Declarations
     (Pkg_Spec : Node_Id;
      Decls    : List_Id);
   --  Add calling stubs to the declarative part

   function Could_Be_Asynchronous (Spec : Node_Id) return Boolean;
   --  Return True if nothing prevents the program whose specification is
   --  given to be asynchronous (i.e. no out parameter).

   function Pack_Entity_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Entity_Id;
      Etyp   : Entity_Id := Empty) return Node_Id;
   --  Pack Object (of type Etyp) into Stream. If Etyp is not given,
   --  then Etype (Object) will be used if present. If the type is
   --  constrained, then 'Write will be used to output the object,
   --  If the type is unconstrained, 'Output will be used.

   function Pack_Node_Into_Stream
     (Loc    : Source_Ptr;
      Stream : Entity_Id;
      Object : Node_Id;
      Etyp   : Entity_Id) return Node_Id;
   --  Similar to above, with an arbitrary node instead of an entity

   function Pack_Node_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Node_Id;
      Etyp   : Entity_Id) return Node_Id;
   --  Similar to above, with Stream instead of Stream'Access

   function Make_Selected_Component
     (Loc           : Source_Ptr;
      Prefix        : Entity_Id;
      Selector_Name : Name_Id) return Node_Id;
   --  Return a selected_component whose prefix denotes the given entity,
   --  and with the given Selector_Name.

   function Scope_Of_Spec (Spec : Node_Id) return Entity_Id;
   --  Return the scope represented by a given spec

   procedure Set_Renaming_TSS
     (Typ     : Entity_Id;
      Nam     : Entity_Id;
      TSS_Nam : TSS_Name_Type);
   --  Create a renaming declaration of subprogram Nam,
   --  and register it as a TSS for Typ with name TSS_Nam.

   function Need_Extra_Constrained (Parameter : Node_Id) return Boolean;
   --  Return True if the current parameter needs an extra formal to reflect
   --  its constrained status.

   function Is_RACW_Controlling_Formal
     (Parameter : Node_Id; Stub_Type : Entity_Id) return Boolean;
   --  Return True if the current parameter is a controlling formal argument
   --  of type Stub_Type or access to Stub_Type.

   procedure Declare_Create_NVList
     (Loc    : Source_Ptr;
      NVList : Entity_Id;
      Decls  : List_Id;
      Stmts  : List_Id);
   --  Append the declaration of NVList to Decls, and its
   --  initialization to Stmts.

   function Add_Parameter_To_NVList
     (Loc         : Source_Ptr;
      NVList      : Entity_Id;
      Parameter   : Entity_Id;
      Constrained : Boolean;
      RACW_Ctrl   : Boolean := False;
      Any         : Entity_Id) return Node_Id;
   --  Return a call to Add_Item to add the Any corresponding
   --  to the designated formal Parameter (with the indicated
   --  Constrained status) to NVList. RACW_Ctrl must be set to
   --  True for controlling formals of distributed object primitive
   --  operations.

   type Stub_Structure is record
      Stub_Type         : Entity_Id;
      Stub_Type_Access  : Entity_Id;
      RPC_Receiver_Decl : Node_Id;
      RACW_Type         : Entity_Id;
   end record;
   --  This structure is necessary because of the two phases analysis of
   --  a RACW declaration occurring in the same Remote_Types package as the
   --  designated type. RACW_Type is any of the RACW types pointing on this
   --  designated type, it is used here to save an anonymous type creation
   --  for each primitive operation.
   --
   --  For a RACW that implements a RAS, no object RPC receiver is generated.
   --  Instead, RPC_Receiver_Decl is the declaration after which the
   --  RPC receiver would have been inserted.

   Empty_Stub_Structure : constant Stub_Structure :=
     (Empty, Empty, Empty, Empty);

   package Stubs_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Stub_Structure,
                         No_Element => Empty_Stub_Structure,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RACW designated type and its stub type

   package Asynchronous_Flags_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Entity_Id,
                         No_Element => Empty,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RACW type and a constant having the value True
   --  if the RACW is asynchronous and False otherwise.

   package RCI_Locator_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Entity_Id,
                         No_Element => Empty,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RCI package on which All_Calls_Remote applies and
   --  the generic instantiation of RCI_Locator for this package.

   package RCI_Calling_Stubs_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Entity_Id,
                         No_Element => Empty,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RCI subprogram and the corresponding calling stubs

   procedure Add_Stub_Type
     (Designated_Type   : Entity_Id;
      RACW_Type         : Entity_Id;
      Decls             : List_Id;
      Stub_Type         : out Entity_Id;
      Stub_Type_Access  : out Entity_Id;
      RPC_Receiver_Decl : out Node_Id;
      Existing          : out Boolean);
   --  Add the declaration of the stub type, the access to stub type and the
   --  object RPC receiver at the end of Decls. If these already exist,
   --  then nothing is added in the tree but the right values are returned
   --  anyhow and Existing is set to True.

   procedure Add_RACW_Asynchronous_Flag
     (Declarations : List_Id;
      RACW_Type    : Entity_Id);
   --  Declare a boolean constant associated with RACW_Type whose value
   --  indicates at run time whether a pragma Asynchronous applies to it.

   procedure Assign_Subprogram_Identifier
     (Def : Entity_Id;
      Spn : Int;
      Id  : out String_Id);
   --  Determine the distribution subprogram identifier to
   --  be used for remote subprogram Def, return it in Id and
   --  store it in a hash table for later retrieval by
   --  Get_Subprogram_Id. Spn is the subprogram number.

   function RCI_Package_Locator
     (Loc          : Source_Ptr;
      Package_Spec : Node_Id) return Node_Id;
   --  Instantiate the generic package RCI_Locator in order to locate the
   --  RCI package whose spec is given as argument.

   function Make_Tag_Check (Loc : Source_Ptr; N : Node_Id) return Node_Id;
   --  Surround a node N by a tag check, as in:
   --      begin
   --         <N>;
   --      exception
   --         when E : Ada.Tags.Tag_Error =>
   --           Raise_Exception (Program_Error'Identity,
   --                            Exception_Message (E));
   --      end;

   function Input_With_Tag_Check
     (Loc      : Source_Ptr;
      Var_Type : Entity_Id;
      Stream   : Node_Id) return Node_Id;
   --  Return a function with the following form:
   --    function R return Var_Type is
   --    begin
   --       return Var_Type'Input (S);
   --    exception
   --       when E : Ada.Tags.Tag_Error =>
   --           Raise_Exception (Program_Error'Identity,
   --                            Exception_Message (E));
   --    end R;

   --------------------------------------------
   -- Hooks for PCS-specific code generation --
   --------------------------------------------

   --  Part of the code generation circuitry for distribution needs to be
   --  tailored for each implementation of the PCS. For each routine that
   --  needs to be specialized, a Specific_<routine> wrapper is created,
   --  which calls the corresponding <routine> in package
   --  <pcs_implementation>_Support.

   procedure Specific_Add_RACW_Features
     (RACW_Type           : Entity_Id;
      Desig               : Entity_Id;
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      RPC_Receiver_Decl   : Node_Id;
      Declarations        : List_Id);
   --  Add declaration for TSSs for a given RACW type. The declarations are
   --  added just after the declaration of the RACW type itself, while the
   --  bodies are inserted at the end of Decls. Runtime-specific ancillary
   --  subprogram for Add_RACW_Features.

   procedure Specific_Add_RAST_Features
     (Vis_Decl : Node_Id;
      RAS_Type : Entity_Id);
   --  Add declaration for TSSs for a given RAS type. PCS-specific ancillary
   --  subprogram for Add_RAST_Features.

   --  An RPC_Target record is used during construction of calling stubs
   --  to pass PCS-specific tree fragments corresponding to the information
   --  necessary to locate the target of a remote subprogram call.

   type RPC_Target (PCS_Kind : PCS_Names) is record
      case PCS_Kind is
         when Name_PolyORB_DSA =>
            Object       : Node_Id;
            --  An expression whose value is a PolyORB reference to the target
            --  object.
         when others           =>
            Partition    : Entity_Id;
            --  A variable containing the Partition_ID of the target parition

            RPC_Receiver : Node_Id;
            --  An expression whose value is the address of the target RPC
            --  receiver.
      end case;
   end record;

   procedure Specific_Build_General_Calling_Stubs
     (Decls                     : List_Id;
      Statements                : List_Id;
      Target                    : RPC_Target;
      Subprogram_Id             : Node_Id;
      Asynchronous              : Node_Id := Empty;
      Is_Known_Asynchronous     : Boolean := False;
      Is_Known_Non_Asynchronous : Boolean := False;
      Is_Function               : Boolean;
      Spec                      : Node_Id;
      Stub_Type                 : Entity_Id := Empty;
      RACW_Type                 : Entity_Id := Empty;
      Nod                       : Node_Id);
   --  Build calling stubs for general purpose. The parameters are:
   --    Decls             : a place to put declarations
   --    Statements        : a place to put statements
   --    Target            : PCS-specific target information (see details
   --                        in RPC_Target declaration).
   --    Subprogram_Id     : a node containing the subprogram ID
   --    Asynchronous      : True if an APC must be made instead of an RPC.
   --                        The value needs not be supplied if one of the
   --                        Is_Known_... is True.
   --    Is_Known_Async... : True if we know that this is asynchronous
   --    Is_Known_Non_A... : True if we know that this is not asynchronous
   --    Spec              : a node with a Parameter_Specifications and
   --                        a Subtype_Mark if applicable
   --    Stub_Type         : in case of RACW stubs, parameters of type access
   --                        to Stub_Type will be marshalled using the
   --                        address of the object (the addr field) rather
   --                        than using the 'Write on the stub itself
   --    Nod               : used to provide sloc for generated code

   function Specific_Build_Stub_Target
     (Loc                   : Source_Ptr;
      Decls                 : List_Id;
      RCI_Locator           : Entity_Id;
      Controlling_Parameter : Entity_Id) return RPC_Target;
   --  Build call target information nodes for use within calling stubs. In the
   --  RCI case, RCI_Locator is the entity for the instance of RCI_Locator. If
   --  for an RACW, Controlling_Parameter is the entity for the controlling
   --  formal parameter used to determine the location of the target of the
   --  call. Decls provides a location where variable declarations can be
   --  appended to construct the necessary values.

   procedure Specific_Build_Stub_Type
     (RACW_Type         : Entity_Id;
      Stub_Type         : Entity_Id;
      Stub_Type_Decl    : out Node_Id;
      RPC_Receiver_Decl : out Node_Id);
   --  Build a type declaration for the stub type associated with an RACW
   --  type, and the necessary RPC receiver, if applicable. PCS-specific
   --  ancillary subprogram for Add_Stub_Type. If no RPC receiver declaration
   --  is generated, then RPC_Receiver_Decl is set to Empty.

   procedure Specific_Build_RPC_Receiver_Body
     (RPC_Receiver : Entity_Id;
      Request      : out Entity_Id;
      Subp_Id      : out Entity_Id;
      Subp_Index   : out Entity_Id;
      Stmts        : out List_Id;
      Decl         : out Node_Id);
   --  Make a subprogram body for an RPC receiver, with the given
   --  defining unit name. On return:
   --    - Subp_Id is the subprogram identifier from the PCS.
   --    - Subp_Index is the index in the list of subprograms
   --      used for dispatching (a variable of type Subprogram_Id).
   --    - Stmts is the place where the request dispatching
   --      statements can occur,
   --    - Decl is the subprogram body declaration.

   function Specific_Build_Subprogram_Receiving_Stubs
     (Vis_Decl                 : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Parent_Primitive         : Entity_Id := Empty) return Node_Id;
   --  Build the receiving stub for a given subprogram. The subprogram
   --  declaration is also built by this procedure, and the value returned
   --  is a N_Subprogram_Body. If a parameter of type access to Stub_Type is
   --  found in the specification, then its address is read from the stream
   --  instead of the object itself and converted into an access to
   --  class-wide type before doing the real call using any of the RACW type
   --  pointing on the designated type.

   procedure Specific_Add_Obj_RPC_Receiver_Completion
     (Loc           : Source_Ptr;
      Decls         : List_Id;
      RPC_Receiver  : Entity_Id;
      Stub_Elements : Stub_Structure);
   --  Add the necessary code to Decls after the completion of generation
   --  of the RACW RPC receiver described by Stub_Elements.

   procedure Specific_Add_Receiving_Stubs_To_Declarations
     (Pkg_Spec : Node_Id;
      Decls    : List_Id);
   --  Add receiving stubs to the declarative part of an RCI unit

   package GARLIC_Support is

      --  Support for generating DSA code that uses the GARLIC PCS

      --  The subprograms below provide the GARLIC versions of
      --  the corresponding Specific_<subprogram> routine declared
      --  above.

      procedure Add_RACW_Features
        (RACW_Type         : Entity_Id;
         Stub_Type         : Entity_Id;
         Stub_Type_Access  : Entity_Id;
         RPC_Receiver_Decl : Node_Id;
         Declarations      : List_Id);

      procedure Add_RAST_Features
        (Vis_Decl : Node_Id;
         RAS_Type : Entity_Id);

      procedure Build_General_Calling_Stubs
        (Decls                     : List_Id;
         Statements                : List_Id;
         Target_Partition          : Entity_Id; --  From RPC_Target
         Target_RPC_Receiver       : Node_Id;   --  From RPC_Target
         Subprogram_Id             : Node_Id;
         Asynchronous              : Node_Id := Empty;
         Is_Known_Asynchronous     : Boolean := False;
         Is_Known_Non_Asynchronous : Boolean := False;
         Is_Function               : Boolean;
         Spec                      : Node_Id;
         Stub_Type                 : Entity_Id := Empty;
         RACW_Type                 : Entity_Id := Empty;
         Nod                       : Node_Id);

      function Build_Stub_Target
        (Loc                   : Source_Ptr;
         Decls                 : List_Id;
         RCI_Locator           : Entity_Id;
         Controlling_Parameter : Entity_Id) return RPC_Target;

      procedure Build_Stub_Type
        (RACW_Type : Entity_Id;
         Stub_Type : Entity_Id;
         Stub_Type_Decl    : out Node_Id;
         RPC_Receiver_Decl : out Node_Id);

      function Build_Subprogram_Receiving_Stubs
        (Vis_Decl                 : Node_Id;
         Asynchronous             : Boolean;
         Dynamically_Asynchronous : Boolean   := False;
         Stub_Type                : Entity_Id := Empty;
         RACW_Type                : Entity_Id := Empty;
         Parent_Primitive         : Entity_Id := Empty) return Node_Id;

      procedure Add_Obj_RPC_Receiver_Completion
        (Loc           : Source_Ptr;
         Decls         : List_Id;
         RPC_Receiver  : Entity_Id;
         Stub_Elements : Stub_Structure);

      procedure Add_Receiving_Stubs_To_Declarations
        (Pkg_Spec : Node_Id;
         Decls    : List_Id);

      procedure Build_RPC_Receiver_Body
        (RPC_Receiver : Entity_Id;
         Request      : out Entity_Id;
         Subp_Id      : out Entity_Id;
         Subp_Index   : out Entity_Id;
         Stmts        : out List_Id;
         Decl         : out Node_Id);

   end GARLIC_Support;

   package PolyORB_Support is

      --  Support for generating DSA code that uses the PolyORB PCS

      --  The subprograms below provide the PolyORB versions of
      --  the corresponding Specific_<subprogram> routine declared
      --  above.

      procedure Add_RACW_Features
        (RACW_Type         : Entity_Id;
         Desig             : Entity_Id;
         Stub_Type         : Entity_Id;
         Stub_Type_Access  : Entity_Id;
         RPC_Receiver_Decl : Node_Id;
         Declarations      : List_Id);

      procedure Add_RAST_Features
        (Vis_Decl : Node_Id;
         RAS_Type : Entity_Id);

      procedure Build_General_Calling_Stubs
        (Decls                     : List_Id;
         Statements                : List_Id;
         Target_Object             : Node_Id; --  From RPC_Target
         Subprogram_Id             : Node_Id;
         Asynchronous              : Node_Id := Empty;
         Is_Known_Asynchronous     : Boolean := False;
         Is_Known_Non_Asynchronous : Boolean := False;
         Is_Function               : Boolean;
         Spec                      : Node_Id;
         Stub_Type                 : Entity_Id := Empty;
         RACW_Type                 : Entity_Id := Empty;
         Nod                       : Node_Id);

      function Build_Stub_Target
        (Loc                   : Source_Ptr;
         Decls                 : List_Id;
         RCI_Locator           : Entity_Id;
         Controlling_Parameter : Entity_Id) return RPC_Target;

      procedure Build_Stub_Type
        (RACW_Type         : Entity_Id;
         Stub_Type         : Entity_Id;
         Stub_Type_Decl    : out Node_Id;
         RPC_Receiver_Decl : out Node_Id);

      function Build_Subprogram_Receiving_Stubs
        (Vis_Decl                 : Node_Id;
         Asynchronous             : Boolean;
         Dynamically_Asynchronous : Boolean   := False;
         Stub_Type                : Entity_Id := Empty;
         RACW_Type                : Entity_Id := Empty;
         Parent_Primitive         : Entity_Id := Empty) return Node_Id;

      procedure Add_Obj_RPC_Receiver_Completion
        (Loc           : Source_Ptr;
         Decls         : List_Id;
         RPC_Receiver  : Entity_Id;
         Stub_Elements : Stub_Structure);

      procedure Add_Receiving_Stubs_To_Declarations
        (Pkg_Spec : Node_Id;
         Decls    : List_Id);

      procedure Build_RPC_Receiver_Body
        (RPC_Receiver : Entity_Id;
         Request      : out Entity_Id;
         Subp_Id      : out Entity_Id;
         Subp_Index   : out Entity_Id;
         Stmts        : out List_Id;
         Decl         : out Node_Id);

      procedure Reserve_NamingContext_Methods;
      --  Mark the method names for interface NamingContext as already used in
      --  the overload table, so no clashes occur with user code (with the
      --  PolyORB PCS, RCIs Implement The NamingContext interface to allow
      --  their methods to be accessed as objects, for the implementation of
      --  remote access-to-subprogram types).

      package Helpers is

         --  Routines to build distribtion helper subprograms for user-defined
         --  types. For implementation of the Distributed systems annex (DSA)
         --  over the PolyORB generic middleware components, it is necessary to
         --  generate several supporting subprograms for each application data
         --  type used in inter-partition communication. These subprograms are:
         --    * a Typecode function returning a high-level description of the
         --      type's structure;
         --    * two conversion functions allowing conversion of values of the
         --      type from and to the generic data containers used by PolyORB.
         --      These generic containers are called 'Any' type values after
         --      the CORBA terminology, and hence the conversion subprograms
         --      are named To_Any and From_Any.

         function Build_From_Any_Call
           (Typ   : Entity_Id;
            N     : Node_Id;
            Decls : List_Id) return Node_Id;
         --  Build call to From_Any attribute function of type Typ with
         --  expression N as actual parameter. Decls is the declarations list
         --  for an appropriate enclosing scope of the point where the call
         --  will be inserted; if the From_Any attribute for Typ needs to be
         --  generated at this point, its declaration is appended to Decls.

         procedure Build_From_Any_Function
           (Loc  : Source_Ptr;
            Typ  : Entity_Id;
            Decl : out Node_Id;
            Fnam : out Entity_Id);
         --  Build From_Any attribute function for Typ. Loc is the reference
         --  location for generated nodes, Typ is the type for which the
         --  conversion function is generated. On return, Decl and Fnam contain
         --  the declaration and entity for the newly-created function.

         function Build_To_Any_Call
           (N     : Node_Id;
            Decls : List_Id) return Node_Id;
         --  Build call to To_Any attribute function with expression as actual
         --  parameter. Decls is the declarations list for an appropriate
         --  enclosing scope of the point where the call will be inserted; if
         --  the To_Any attribute for Typ needs to be generated at this point,
         --  its declaration is appended to Decls.

         procedure Build_To_Any_Function
           (Loc  : Source_Ptr;
            Typ  : Entity_Id;
            Decl : out Node_Id;
            Fnam : out Entity_Id);
         --  Build To_Any attribute function for Typ. Loc is the reference
         --  location for generated nodes, Typ is the type for which the
         --  conversion function is generated. On return, Decl and Fnam contain
         --  the declaration and entity for the newly-created function.

         function Build_TypeCode_Call
           (Loc   : Source_Ptr;
            Typ   : Entity_Id;
            Decls : List_Id) return Node_Id;
         --  Build call to TypeCode attribute function for Typ. Decls is the
         --  declarations list for an appropriate enclosing scope of the point
         --  where the call will be inserted; if the To_Any attribute for Typ
         --  needs to be generated at this point, its declaration is appended
         --  to Decls.

         procedure Build_TypeCode_Function
           (Loc  : Source_Ptr;
            Typ  : Entity_Id;
            Decl : out Node_Id;
            Fnam : out Entity_Id);
         --  Build TypeCode attribute function for Typ. Loc is the reference
         --  location for generated nodes, Typ is the type for which the
         --  conversion function is generated. On return, Decl and Fnam contain
         --  the declaration and entity for the newly-created function.

         procedure Build_Name_And_Repository_Id
           (E           : Entity_Id;
            Name_Str    : out String_Id;
            Repo_Id_Str : out String_Id);
         --  In the PolyORB distribution model, each distributed object type
         --  and each distributed operation has a globally unique identifier,
         --  its Repository Id. This subprogram builds and returns two strings
         --  for entity E (a distributed object type or operation): one
         --  containing the name of E, the second containing its repository id.

      end Helpers;

   end PolyORB_Support;

   ------------------------------------
   -- Local variables and structures --
   ------------------------------------

   RCI_Cache : Node_Id;
   --  Needs comments ???

   Output_From_Constrained : constant array (Boolean) of Name_Id :=
     (False => Name_Output,
      True  => Name_Write);
   --  The attribute to choose depending on the fact that the parameter
   --  is constrained or not. There is no such thing as Input_From_Constrained
   --  since this require separate mechanisms ('Input is a function while
   --  'Read is a procedure).

   ---------------------------------------
   -- Add_Calling_Stubs_To_Declarations --
   ---------------------------------------

   procedure Add_Calling_Stubs_To_Declarations
     (Pkg_Spec : Node_Id;
      Decls    : List_Id)
   is
      Current_Subprogram_Number : Int := First_RCI_Subprogram_Id;
      --  Subprogram id 0 is reserved for calls received from
      --  remote access-to-subprogram dereferences.

      Current_Declaration       : Node_Id;
      Loc                       : constant Source_Ptr := Sloc (Pkg_Spec);
      RCI_Instantiation         : Node_Id;
      Subp_Stubs                : Node_Id;
      Subp_Str                  : String_Id;

   begin
      --  The first thing added is an instantiation of the generic package
      --  System.Partition_Interface.RCI_Locator with the name of this
      --  remote package. This will act as an interface with the name server
      --  to determine the Partition_ID and the RPC_Receiver for the
      --  receiver of this package.

      RCI_Instantiation := RCI_Package_Locator (Loc, Pkg_Spec);
      RCI_Cache         := Defining_Unit_Name (RCI_Instantiation);

      Append_To (Decls, RCI_Instantiation);
      Analyze (RCI_Instantiation);

      --  For each subprogram declaration visible in the spec, we do
      --  build a body. We also increment a counter to assign a different
      --  Subprogram_Id to each subprograms. The receiving stubs processing
      --  do use the same mechanism and will thus assign the same Id and
      --  do the correct dispatching.

      Overload_Counter_Table.Reset;
      PolyORB_Support.Reserve_NamingContext_Methods;

      Current_Declaration := First (Visible_Declarations (Pkg_Spec));

      while Present (Current_Declaration) loop
         if Nkind (Current_Declaration) = N_Subprogram_Declaration
           and then Comes_From_Source (Current_Declaration)
         then
            Assign_Subprogram_Identifier (
              Defining_Unit_Name (Specification (Current_Declaration)),
              Current_Subprogram_Number,
              Subp_Str);

            Subp_Stubs :=
              Build_Subprogram_Calling_Stubs (
                Vis_Decl     => Current_Declaration,
                Subp_Id      =>
                  Build_Subprogram_Id (Loc,
                    Defining_Unit_Name (Specification (Current_Declaration))),
                Asynchronous =>
                  Nkind (Specification (Current_Declaration)) =
                    N_Procedure_Specification
                  and then
                    Is_Asynchronous (Defining_Unit_Name (Specification
                      (Current_Declaration))));

            Append_To (Decls, Subp_Stubs);
            Analyze (Subp_Stubs);

            Current_Subprogram_Number := Current_Subprogram_Number + 1;
         end if;

         Next (Current_Declaration);
      end loop;
   end Add_Calling_Stubs_To_Declarations;

   -----------------------------
   -- Add_Parameter_To_NVList --
   -----------------------------

   function Add_Parameter_To_NVList
     (Loc         : Source_Ptr;
      NVList      : Entity_Id;
      Parameter   : Entity_Id;
      Constrained : Boolean;
      RACW_Ctrl   : Boolean := False;
      Any         : Entity_Id) return Node_Id
   is
      Parameter_Name_String : String_Id;
      Parameter_Mode        : Node_Id;

      function Parameter_Passing_Mode
        (Loc         : Source_Ptr;
         Parameter   : Entity_Id;
         Constrained : Boolean) return Node_Id;
      --  Return an expression that denotes the parameter passing
      --  mode to be used for Parameter in distribution stubs,
      --  where Constrained is Parameter's constrained status.

      ----------------------------
      -- Parameter_Passing_Mode --
      ----------------------------

      function Parameter_Passing_Mode
        (Loc         : Source_Ptr;
         Parameter   : Entity_Id;
         Constrained : Boolean) return Node_Id
      is
         Lib_RE : RE_Id;

      begin
         if Out_Present (Parameter) then
            if In_Present (Parameter)
              or else not Constrained
            then
               --  Unconstrained formals must be translated
               --  to 'in' or 'inout', not 'out', because
               --  they need to be constrained by the actual.

               Lib_RE := RE_Mode_Inout;
            else
               Lib_RE := RE_Mode_Out;
            end if;

         else
            Lib_RE := RE_Mode_In;
         end if;

         return New_Occurrence_Of (RTE (Lib_RE), Loc);
      end Parameter_Passing_Mode;

   --  Start of processing for Add_Parameter_To_NVList

   begin
      if Nkind (Parameter) = N_Defining_Identifier then
         Get_Name_String (Chars (Parameter));
      else
         Get_Name_String (Chars (Defining_Identifier
                                  (Parameter)));
      end if;

      Parameter_Name_String := String_From_Name_Buffer;

      if RACW_Ctrl then
         Parameter_Mode := New_Occurrence_Of
           (RTE (RE_Mode_In), Loc);
      else
         Parameter_Mode := Parameter_Passing_Mode (Loc,
           Parameter, Constrained);
      end if;

      return
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of
              (RTE (RE_NVList_Add_Item), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (NVList, Loc),
            Make_Function_Call (Loc,
              Name =>
                New_Occurrence_Of
                  (RTE (RE_To_PolyORB_String), Loc),
              Parameter_Associations => New_List (
                Make_String_Literal (Loc,
                  Strval => Parameter_Name_String))),
            New_Occurrence_Of (Any, Loc),
            Parameter_Mode));
   end Add_Parameter_To_NVList;

   --------------------------------
   -- Add_RACW_Asynchronous_Flag --
   --------------------------------

   procedure Add_RACW_Asynchronous_Flag
     (Declarations : List_Id;
      RACW_Type    : Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

      Asynchronous_Flag : constant Entity_Id :=
                            Make_Defining_Identifier (Loc,
                              New_External_Name (Chars (RACW_Type), 'A'));

   begin
      --  Declare the asynchronous flag. This flag will be changed to True
      --  whenever it is known that the RACW type is asynchronous.

      Append_To (Declarations,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Asynchronous_Flag,
          Constant_Present    => True,
          Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          => New_Occurrence_Of (Standard_False, Loc)));

      Asynchronous_Flags_Table.Set (RACW_Type, Asynchronous_Flag);
   end Add_RACW_Asynchronous_Flag;

   -----------------------
   -- Add_RACW_Features --
   -----------------------

   procedure Add_RACW_Features (RACW_Type : Entity_Id)
   is
      Desig : constant Entity_Id :=
                Etype (Designated_Type (RACW_Type));
      Decls : List_Id :=
                List_Containing (Declaration_Node (RACW_Type));

      Same_Scope : constant Boolean :=
                     Scope (Desig) = Scope (RACW_Type);

      Stub_Type         : Entity_Id;
      Stub_Type_Access  : Entity_Id;
      RPC_Receiver_Decl : Node_Id;
      Existing          : Boolean;

   begin
      if not Expander_Active then
         return;
      end if;

      if Same_Scope then

         --  We are declaring a RACW in the same package than its designated
         --  type, so the list to use for late declarations must be the
         --  private part of the package. We do know that this private part
         --  exists since the designated type has to be a private one.

         Decls := Private_Declarations
           (Package_Specification_Of_Scope (Current_Scope));

      elsif Nkind (Parent (Decls)) = N_Package_Specification
        and then Present (Private_Declarations (Parent (Decls)))
      then
         Decls := Private_Declarations (Parent (Decls));
      end if;

      --  If we were unable to find the declarations, that means that the
      --  completion of the type was missing. We can safely return and let
      --  the error be caught by the semantic analysis.

      if No (Decls) then
         return;
      end if;

      Add_Stub_Type
        (Designated_Type     => Desig,
         RACW_Type           => RACW_Type,
         Decls               => Decls,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         RPC_Receiver_Decl   => RPC_Receiver_Decl,
         Existing            => Existing);

      Add_RACW_Asynchronous_Flag
        (Declarations        => Decls,
         RACW_Type           => RACW_Type);

      Specific_Add_RACW_Features
        (RACW_Type           => RACW_Type,
         Desig               => Desig,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         RPC_Receiver_Decl   => RPC_Receiver_Decl,
         Declarations        => Decls);

      if not Same_Scope and then not Existing then

         --  The RACW has been declared in another scope than the designated
         --  type and has not been handled by another RACW in the same package
         --  as the first one, so add primitive for the stub type here.

         Add_RACW_Primitive_Declarations_And_Bodies
           (Designated_Type  => Desig,
            Insertion_Node   => RPC_Receiver_Decl,
            Decls            => Decls);

      else
         Add_Access_Type_To_Process (E => Desig, A => RACW_Type);
      end if;
   end Add_RACW_Features;

   ------------------------------------------------
   -- Add_RACW_Primitive_Declarations_And_Bodies --
   ------------------------------------------------

   procedure Add_RACW_Primitive_Declarations_And_Bodies
     (Designated_Type : Entity_Id;
      Insertion_Node  : Node_Id;
      Decls           : List_Id)
   is
      --  Set Sloc of generated declaration copy of insertion node Sloc, so
      --  the declarations are recognized as belonging to the current package.

      Loc : constant Source_Ptr := Sloc (Insertion_Node);

      Stub_Elements : constant Stub_Structure :=
                        Stubs_Table.Get (Designated_Type);

      pragma Assert (Stub_Elements /= Empty_Stub_Structure);
      Is_RAS : constant Boolean :=
        not Comes_From_Source (Stub_Elements.RACW_Type);

      Current_Insertion_Node : Node_Id := Insertion_Node;

      RPC_Receiver                   : Entity_Id;
      RPC_Receiver_Statements        : List_Id;
      RPC_Receiver_Case_Alternatives : constant List_Id := New_List;
      RPC_Receiver_Elsif_Parts       : List_Id;
      RPC_Receiver_Request           : Entity_Id;
      RPC_Receiver_Subp_Id           : Entity_Id;
      RPC_Receiver_Subp_Index        : Entity_Id;

      Subp_Str : String_Id;

      Current_Primitive_Elmt   : Elmt_Id;
      Current_Primitive        : Entity_Id;
      Current_Primitive_Body   : Node_Id;
      Current_Primitive_Spec   : Node_Id;
      Current_Primitive_Decl   : Node_Id;
      Current_Primitive_Number : Int := 0;

      Current_Primitive_Alias : Node_Id;

      Current_Receiver      : Entity_Id;
      Current_Receiver_Body : Node_Id;

      RPC_Receiver_Decl : Node_Id;

      Possibly_Asynchronous : Boolean;

   begin
      if not Expander_Active then
         return;
      end if;

      if not Is_RAS then
         RPC_Receiver := Make_Defining_Identifier (Loc,
                           New_Internal_Name ('P'));
         Specific_Build_RPC_Receiver_Body (
           RPC_Receiver => RPC_Receiver,
           Request      => RPC_Receiver_Request,
           Subp_Id      => RPC_Receiver_Subp_Id,
           Subp_Index   => RPC_Receiver_Subp_Index,
           Stmts        => RPC_Receiver_Statements,
           Decl         => RPC_Receiver_Decl);

         if Get_PCS_Name = Name_PolyORB_DSA then

            --  For the case of PolyORB, we need to map a textual operation
            --  name into a primitive index. Currently we do so using a
            --  simple sequence of string comparisons.

            RPC_Receiver_Elsif_Parts := New_List;
            Append_To (RPC_Receiver_Statements,
              Make_Implicit_If_Statement (Designated_Type,
                Condition       => New_Occurrence_Of (Standard_False, Loc),
                Then_Statements => New_List,
                Elsif_Parts     => RPC_Receiver_Elsif_Parts));
         end if;
      end if;

      --  Build callers, receivers for every primitive operations and a RPC
      --  receiver for this type.

      if Present (Primitive_Operations (Designated_Type)) then
         Overload_Counter_Table.Reset;

         Current_Primitive_Elmt :=
           First_Elmt (Primitive_Operations (Designated_Type));
         while Current_Primitive_Elmt /= No_Elmt loop
            Current_Primitive := Node (Current_Primitive_Elmt);

            --  Copy the primitive of all the parents, except predefined
            --  ones that are not remotely dispatching.

            if Chars (Current_Primitive) /= Name_uSize
              and then Chars (Current_Primitive) /= Name_uAlignment
              and then not Is_TSS (Current_Primitive, TSS_Deep_Finalize)
            then
               --  The first thing to do is build an up-to-date copy of
               --  the spec with all the formals referencing Designated_Type
               --  transformed into formals referencing Stub_Type. Since this
               --  primitive may have been inherited, go back the alias chain
               --  until the real primitive has been found.

               Current_Primitive_Alias := Current_Primitive;
               while Present (Alias (Current_Primitive_Alias)) loop
                  pragma Assert
                    (Current_Primitive_Alias
                      /= Alias (Current_Primitive_Alias));
                  Current_Primitive_Alias := Alias (Current_Primitive_Alias);
               end loop;

               Current_Primitive_Spec :=
                 Copy_Specification (Loc,
                   Spec        => Parent (Current_Primitive_Alias),
                   Object_Type => Designated_Type,
                   Stub_Type   => Stub_Elements.Stub_Type);

               Current_Primitive_Decl :=
                 Make_Subprogram_Declaration (Loc,
                   Specification => Current_Primitive_Spec);

               Insert_After (Current_Insertion_Node, Current_Primitive_Decl);
               Analyze (Current_Primitive_Decl);
               Current_Insertion_Node := Current_Primitive_Decl;

               Possibly_Asynchronous :=
                 Nkind (Current_Primitive_Spec) = N_Procedure_Specification
                 and then Could_Be_Asynchronous (Current_Primitive_Spec);

               Assign_Subprogram_Identifier (
                 Defining_Unit_Name (Current_Primitive_Spec),
                 Current_Primitive_Number,
                 Subp_Str);

               Current_Primitive_Body :=
                 Build_Subprogram_Calling_Stubs
                   (Vis_Decl                 => Current_Primitive_Decl,
                    Subp_Id                  =>
                      Build_Subprogram_Id (Loc,
                        Defining_Unit_Name (Current_Primitive_Spec)),
                    Asynchronous             => Possibly_Asynchronous,
                    Dynamically_Asynchronous => Possibly_Asynchronous,
                    Stub_Type                => Stub_Elements.Stub_Type,
                    RACW_Type                => Stub_Elements.RACW_Type);
               Append_To (Decls, Current_Primitive_Body);

               --  Analyzing the body here would cause the Stub type to be
               --  frozen, thus preventing subsequent primitive declarations.
               --  For this reason, it will be analyzed later in the
               --  regular flow.

               --  Build the receiver stubs

               if not Is_RAS then
                  Current_Receiver_Body :=
                    Specific_Build_Subprogram_Receiving_Stubs
                      (Vis_Decl                 => Current_Primitive_Decl,
                       Asynchronous             => Possibly_Asynchronous,
                       Dynamically_Asynchronous => Possibly_Asynchronous,
                       Stub_Type                => Stub_Elements.Stub_Type,
                       RACW_Type                => Stub_Elements.RACW_Type,
                       Parent_Primitive         => Current_Primitive);

                  Current_Receiver := Defining_Unit_Name (
                    Specification (Current_Receiver_Body));

                  Append_To (Decls, Current_Receiver_Body);

                  --  Add a case alternative to the receiver

                  if Get_PCS_Name = Name_PolyORB_DSA then
                     Append_To (RPC_Receiver_Elsif_Parts,
                       Make_Elsif_Part (Loc,
                         Condition =>
                           Make_Function_Call (Loc,
                             Name =>
                               New_Occurrence_Of (
                                 RTE (RE_Caseless_String_Eq), Loc),
                             Parameter_Associations => New_List (
                               New_Occurrence_Of (RPC_Receiver_Subp_Id, Loc),
                               Make_String_Literal (Loc, Subp_Str))),
                         Then_Statements => New_List (
                           Make_Assignment_Statement (Loc,
                             Name => New_Occurrence_Of (
                                       RPC_Receiver_Subp_Index, Loc),
                             Expression =>
                               Make_Integer_Literal (Loc,
                                  Current_Primitive_Number)))));
                  end if;

                  Append_To (RPC_Receiver_Case_Alternatives,
                    Make_Case_Statement_Alternative (Loc,
                      Discrete_Choices => New_List (
                        Make_Integer_Literal (Loc, Current_Primitive_Number)),

                      Statements       => New_List (
                        Make_Procedure_Call_Statement (Loc,
                          Name                   =>
                            New_Occurrence_Of (Current_Receiver, Loc),
                          Parameter_Associations => New_List (
                            New_Occurrence_Of (RPC_Receiver_Request, Loc))))));
               end if;

               --  Increment the index of current primitive

               Current_Primitive_Number := Current_Primitive_Number + 1;
            end if;

            Next_Elmt (Current_Primitive_Elmt);
         end loop;
      end if;

      --  Build the case statement and the heart of the subprogram

      if not Is_RAS then
         Append_To (RPC_Receiver_Case_Alternatives,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices => New_List (Make_Others_Choice (Loc)),
             Statements       => New_List (Make_Null_Statement (Loc))));

         Append_To (RPC_Receiver_Statements,
           Make_Case_Statement (Loc,
             Expression   =>
               New_Occurrence_Of (RPC_Receiver_Subp_Index, Loc),
             Alternatives => RPC_Receiver_Case_Alternatives));

         Append_To (Decls, RPC_Receiver_Decl);
         Specific_Add_Obj_RPC_Receiver_Completion (Loc,
           Decls, RPC_Receiver, Stub_Elements);
      end if;

      --  Do not analyze RPC receiver at this stage since it will otherwise
      --  reference subprograms that have not been analyzed yet. It will
      --  be analyzed in the regular flow.

   end Add_RACW_Primitive_Declarations_And_Bodies;

   -----------------------------
   -- Add_RAS_Dereference_TSS --
   -----------------------------

   procedure Add_RAS_Dereference_TSS (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Type_Def : constant Node_Id   := Type_Definition (N);

      RAS_Type  : constant Entity_Id := Defining_Identifier (N);
      Fat_Type  : constant Entity_Id := Equivalent_Type (RAS_Type);
      RACW_Type : constant Entity_Id := Underlying_RACW_Type (RAS_Type);
      Desig     : constant Entity_Id := Etype (Designated_Type (RACW_Type));

      Stub_Elements : constant Stub_Structure := Stubs_Table.Get (Desig);
      pragma Assert (Stub_Elements /= Empty_Stub_Structure);

      RACW_Primitive_Name : Node_Id;

      Proc : constant Entity_Id :=
               Make_Defining_Identifier (Loc,
                 Chars => Make_TSS_Name (RAS_Type, TSS_RAS_Dereference));

      Proc_Spec   : Node_Id;
      Param_Specs : List_Id;
      Param_Assoc : constant List_Id := New_List;
      Stmts       : constant List_Id := New_List;

      RAS_Parameter : constant Entity_Id :=
                        Make_Defining_Identifier (Loc,
                          Chars => New_Internal_Name ('P'));

      Is_Function : constant Boolean :=
                      Nkind (Type_Def) = N_Access_Function_Definition;

      Is_Degenerate : Boolean;
      --  Set to True if the subprogram_specification for this RAS has
      --  an anonymous access parameter (see Process_Remote_AST_Declaration).

      Spec : constant Node_Id := Type_Def;

      Current_Parameter : Node_Id;

   --  Start of processing for Add_RAS_Dereference_TSS

   begin
      --  The Dereference TSS for a remote access-to-subprogram type
      --  has the form:

      --    [function|procedure] ras_typeRD (RAS_Value, <RAS_Parameters>)
      --       [return <>]

      --  This is called whenever a value of a RAS type is dereferenced

      --  First construct a list of parameter specifications:

      --  The first formal is the RAS values

      Param_Specs := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => RAS_Parameter,
          In_Present          => True,
          Parameter_Type      =>
            New_Occurrence_Of (Fat_Type, Loc)));

      --  The following formals are copied from the type declaration

      Is_Degenerate := False;
      Current_Parameter := First (Parameter_Specifications (Type_Def));
      Parameters : while Present (Current_Parameter) loop
         if Nkind (Parameter_Type (Current_Parameter))
           = N_Access_Definition
         then
            Is_Degenerate := True;
         end if;
         Append_To (Param_Specs,
           Make_Parameter_Specification (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc,
                 Chars => Chars (Defining_Identifier (Current_Parameter))),
             In_Present        => In_Present (Current_Parameter),
             Out_Present       => Out_Present (Current_Parameter),
             Parameter_Type    =>
               New_Copy_Tree (Parameter_Type (Current_Parameter)),
             Expression        =>
               New_Copy_Tree (Expression (Current_Parameter))));

         Append_To (Param_Assoc,
           Make_Identifier (Loc,
             Chars => Chars (Defining_Identifier (Current_Parameter))));

         Next (Current_Parameter);
      end loop Parameters;

      if Is_Degenerate then
         Prepend_To (Param_Assoc, New_Occurrence_Of (RAS_Parameter, Loc));

         --  Generate a dummy body. This code will never actually be executed,
         --  because null is the only legal value for a degenerate RAS type.
         --  For legality's sake (in order to avoid generating a function
         --  that does not contain a return statement), we include a dummy
         --  recursive call on the TSS itself.

         Append_To (Stmts,
           Make_Raise_Program_Error (Loc, Reason => PE_Explicit_Raise));
         RACW_Primitive_Name := New_Occurrence_Of (Proc, Loc);

      else
         --  For a normal RAS type, we cast the RAS formal to the corresponding
         --  tagged type, and perform a dispatching call to its Call
         --  primitive operation.

         Prepend_To (Param_Assoc,
           Unchecked_Convert_To (RACW_Type,
             New_Occurrence_Of (RAS_Parameter, Loc)));

         RACW_Primitive_Name := Make_Selected_Component (Loc,
                                  Prefix        => Scope (RACW_Type),
                                  Selector_Name => Name_Call);
      end if;

      if Is_Function then
         Append_To (Stmts,
            Make_Return_Statement (Loc,
              Expression =>
                Make_Function_Call (Loc,
              Name                   =>
                RACW_Primitive_Name,
              Parameter_Associations => Param_Assoc)));

      else
         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               RACW_Primitive_Name,
             Parameter_Associations => Param_Assoc));
      end if;

      --  Build the complete subprogram

      if Is_Function then
         Proc_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Proc,
             Parameter_Specifications => Param_Specs,
             Subtype_Mark             =>
               New_Occurrence_Of (
                 Entity (Subtype_Mark (Spec)), Loc));

         Set_Ekind (Proc, E_Function);
         Set_Etype (Proc,
           New_Occurrence_Of (Entity (Subtype_Mark (Spec)), Loc));

      else
         Proc_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       => Proc,
             Parameter_Specifications => Param_Specs);

         Set_Ekind (Proc, E_Procedure);
         Set_Etype (Proc, Standard_Void_Type);
      end if;

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification              => Proc_Spec,
          Declarations               => New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts)));

      Set_TSS (Fat_Type, Proc);
   end Add_RAS_Dereference_TSS;

   -------------------------------
   -- Add_RAS_Proxy_And_Analyze --
   -------------------------------

   procedure Add_RAS_Proxy_And_Analyze
     (Decls              : List_Id;
      Vis_Decl           : Node_Id;
      All_Calls_Remote_E : Entity_Id;
      Proxy_Object_Addr  : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Vis_Decl);

      Subp_Name : constant Entity_Id :=
                     Defining_Unit_Name (Specification (Vis_Decl));

      Pkg_Name   : constant Entity_Id :=
                     Make_Defining_Identifier (Loc,
                       Chars =>
                         New_External_Name (Chars (Subp_Name), 'P', -1));

      Proxy_Type : constant Entity_Id :=
                     Make_Defining_Identifier (Loc,
                       Chars =>
                         New_External_Name (
                           Related_Id => Chars (Subp_Name),
                           Suffix     => 'P'));

      Proxy_Type_Full_View : constant Entity_Id :=
                               Make_Defining_Identifier (Loc,
                                 Chars (Proxy_Type));

      Subp_Decl_Spec : constant Node_Id :=
                         Build_RAS_Primitive_Specification
                           (Subp_Spec          => Specification (Vis_Decl),
                            Remote_Object_Type => Proxy_Type);

      Subp_Body_Spec : constant Node_Id :=
                         Build_RAS_Primitive_Specification
                           (Subp_Spec          => Specification (Vis_Decl),
                            Remote_Object_Type => Proxy_Type);

      Vis_Decls    : constant List_Id := New_List;
      Pvt_Decls    : constant List_Id := New_List;
      Actuals      : constant List_Id := New_List;
      Formal       : Node_Id;
      Perform_Call : Node_Id;

   begin
      --  type subpP is tagged limited private;

      Append_To (Vis_Decls,
        Make_Private_Type_Declaration (Loc,
          Defining_Identifier => Proxy_Type,
          Tagged_Present      => True,
          Limited_Present     => True));

      --  [subprogram] Call
      --    (Self : access subpP;
      --     ...other-formals...)
      --     [return T];

      Append_To (Vis_Decls,
        Make_Subprogram_Declaration (Loc,
          Specification => Subp_Decl_Spec));

      --  A : constant System.Address;

      Proxy_Object_Addr := Make_Defining_Identifier (Loc, Name_uA);

      Append_To (Vis_Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Proxy_Object_Addr,
          Constant_Present     =>
            True,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Address), Loc)));

      --  private

      --  type subpP is tagged limited record
      --     All_Calls_Remote : Boolean := [All_Calls_Remote?];
      --     ...
      --  end record;

      Append_To (Pvt_Decls,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier =>
            Proxy_Type_Full_View,
          Type_Definition     =>
            Build_Remote_Subprogram_Proxy_Type (Loc,
              New_Occurrence_Of (All_Calls_Remote_E, Loc))));

      --  Trick semantic analysis into swapping the public and
      --  full view when freezing the public view.

      Set_Comes_From_Source (Proxy_Type_Full_View, True);

      --  procedure Call
      --    (Self : access O;
      --     ...other-formals...) is
      --  begin
      --    P (...other-formals...);
      --  end Call;

      --  function Call
      --    (Self : access O;
      --     ...other-formals...)
      --     return T is
      --  begin
      --    return F (...other-formals...);
      --  end Call;

      if Nkind (Subp_Decl_Spec) = N_Procedure_Specification then
         Perform_Call :=
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (Subp_Name, Loc),
             Parameter_Associations =>
               Actuals);
      else
         Perform_Call :=
           Make_Return_Statement (Loc,
             Expression =>
           Make_Function_Call (Loc,
             Name =>
               New_Occurrence_Of (Subp_Name, Loc),
             Parameter_Associations =>
               Actuals));
      end if;

      Formal := First (Parameter_Specifications (Subp_Decl_Spec));
      pragma Assert (Present (Formal));
      loop
         Next (Formal);
         exit when No (Formal);
         Append_To (Actuals,
           New_Occurrence_Of (Defining_Identifier (Formal), Loc));
      end loop;

      --  O : aliased subpP;

      Append_To (Pvt_Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc,
              Name_uO),
          Aliased_Present =>
            True,
          Object_Definition =>
            New_Occurrence_Of (Proxy_Type, Loc)));

      --  A : constant System.Address := O'Address;

      Append_To (Pvt_Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc,
              Chars (Proxy_Object_Addr)),
          Constant_Present =>
            True,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Address), Loc),
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Occurrence_Of (
                Defining_Identifier (Last (Pvt_Decls)), Loc),
              Attribute_Name =>
                Name_Address)));

      Append_To (Decls,
        Make_Package_Declaration (Loc,
          Specification => Make_Package_Specification (Loc,
            Defining_Unit_Name   => Pkg_Name,
            Visible_Declarations => Vis_Decls,
            Private_Declarations => Pvt_Decls,
            End_Label            => Empty)));
      Analyze (Last (Decls));

      Append_To (Decls,
        Make_Package_Body (Loc,
          Defining_Unit_Name =>
            Make_Defining_Identifier (Loc,
              Chars (Pkg_Name)),
          Declarations => New_List (
            Make_Subprogram_Body (Loc,
              Specification  =>
                Subp_Body_Spec,
              Declarations   => New_List,
              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => New_List (Perform_Call))))));
      Analyze (Last (Decls));
   end Add_RAS_Proxy_And_Analyze;

   -----------------------
   -- Add_RAST_Features --
   -----------------------

   procedure Add_RAST_Features (Vis_Decl : Node_Id) is
      RAS_Type : constant Entity_Id :=
                   Equivalent_Type (Defining_Identifier (Vis_Decl));
   begin
      pragma Assert (No (TSS (RAS_Type, TSS_RAS_Access)));
      Add_RAS_Dereference_TSS (Vis_Decl);
      Specific_Add_RAST_Features (Vis_Decl, RAS_Type);
   end Add_RAST_Features;

   -------------------
   -- Add_Stub_Type --
   -------------------

   procedure Add_Stub_Type
     (Designated_Type   : Entity_Id;
      RACW_Type         : Entity_Id;
      Decls             : List_Id;
      Stub_Type         : out Entity_Id;
      Stub_Type_Access  : out Entity_Id;
      RPC_Receiver_Decl : out Node_Id;
      Existing          : out Boolean)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

      Stub_Elements : constant Stub_Structure :=
                        Stubs_Table.Get (Designated_Type);
      Stub_Type_Decl        : Node_Id;
      Stub_Type_Access_Decl : Node_Id;

   begin
      if Stub_Elements /= Empty_Stub_Structure then
         Stub_Type           := Stub_Elements.Stub_Type;
         Stub_Type_Access    := Stub_Elements.Stub_Type_Access;
         RPC_Receiver_Decl   := Stub_Elements.RPC_Receiver_Decl;
         Existing            := True;
         return;
      end if;

      Existing         := False;
      Stub_Type        :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Stub_Type_Access :=
        Make_Defining_Identifier (Loc,
          New_External_Name (
            Related_Id => Chars (Stub_Type),
            Suffix     => 'A'));

      Specific_Build_Stub_Type (
        RACW_Type, Stub_Type,
        Stub_Type_Decl, RPC_Receiver_Decl);

      Stub_Type_Access_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Stub_Type_Access,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication => New_Occurrence_Of (Stub_Type, Loc)));

      Append_To (Decls, Stub_Type_Decl);
      Analyze (Last (Decls));
      Append_To (Decls, Stub_Type_Access_Decl);
      Analyze (Last (Decls));

      --  This is in no way a type derivation, but we fake it to make
      --  sure that the dispatching table gets built with the corresponding
      --  primitive operations at the right place.

      Derive_Subprograms (Parent_Type  => Designated_Type,
                          Derived_Type => Stub_Type);

      if Present (RPC_Receiver_Decl) then
         Append_To (Decls, RPC_Receiver_Decl);
      else
         RPC_Receiver_Decl := Last (Decls);
      end if;

      Stubs_Table.Set (Designated_Type,
        (Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         RPC_Receiver_Decl   => RPC_Receiver_Decl,
         RACW_Type           => RACW_Type));
   end Add_Stub_Type;

   ----------------------------------
   -- Assign_Subprogram_Identifier --
   ----------------------------------

   procedure Assign_Subprogram_Identifier
     (Def : Entity_Id;
      Spn : Int;
      Id  : out String_Id)
   is
      N : constant Name_Id := Chars (Def);

      Overload_Order : constant Int :=
                         Overload_Counter_Table.Get (N) + 1;

   begin
      Overload_Counter_Table.Set (N, Overload_Order);

      Get_Name_String (N);

      --  Homonym handling: as in Exp_Dbug, but much simpler,
      --  because the only entities for which we have to generate
      --  names here need only to be disambiguated within their
      --  own scope.

      if Overload_Order > 1 then
         Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "__";
         Name_Len := Name_Len + 2;
         Add_Nat_To_Name_Buffer (Overload_Order);
      end if;

      Id := String_From_Name_Buffer;
      Subprogram_Identifier_Table.Set (Def,
        Subprogram_Identifiers'(Str_Identifier => Id, Int_Identifier => Spn));
   end Assign_Subprogram_Identifier;

   ------------------------------
   -- Build_Get_Unique_RP_Call --
   ------------------------------

   function Build_Get_Unique_RP_Call
     (Loc       : Source_Ptr;
      Pointer   : Entity_Id;
      Stub_Type : Entity_Id) return List_Id
   is
   begin
      return New_List (
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Get_Unique_Remote_Pointer), Loc),
          Parameter_Associations => New_List (
            Unchecked_Convert_To (RTE (RE_RACW_Stub_Type_Access),
              New_Occurrence_Of (Pointer, Loc)))),

        Make_Assignment_Statement (Loc,
          Name =>
            Make_Selected_Component (Loc,
              Prefix =>
                New_Occurrence_Of (Pointer, Loc),
              Selector_Name =>
                New_Occurrence_Of (First_Tag_Component
                  (Designated_Type (Etype (Pointer))), Loc)),
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix =>
                New_Occurrence_Of (Stub_Type, Loc),
              Attribute_Name =>
                Name_Tag)));

      --  Note: The assignment to Pointer._Tag is safe here because
      --  we carefully ensured that Stub_Type has exactly the same layout
      --  as System.Partition_Interface.RACW_Stub_Type.

   end Build_Get_Unique_RP_Call;

   -----------------------------------
   -- Build_Ordered_Parameters_List --
   -----------------------------------

   function Build_Ordered_Parameters_List (Spec : Node_Id) return List_Id is
      Constrained_List   : List_Id;
      Unconstrained_List : List_Id;
      Current_Parameter  : Node_Id;

      First_Parameter : Node_Id;
      For_RAS         : Boolean := False;

   begin
      if not Present (Parameter_Specifications (Spec)) then
         return New_List;
      end if;

      Constrained_List   := New_List;
      Unconstrained_List := New_List;
      First_Parameter    := First (Parameter_Specifications (Spec));

      if Nkind (Parameter_Type (First_Parameter)) = N_Access_Definition
        and then Chars (Defining_Identifier (First_Parameter)) = Name_uS
      then
         For_RAS := True;
      end if;

      --  Loop through the parameters and add them to the right list

      Current_Parameter := First_Parameter;
      while Present (Current_Parameter) loop
         if (Nkind (Parameter_Type (Current_Parameter)) = N_Access_Definition
             or else
               Is_Constrained (Etype (Parameter_Type (Current_Parameter)))
             or else
               Is_Elementary_Type (Etype (Parameter_Type (Current_Parameter))))
           and then not (For_RAS and then Current_Parameter = First_Parameter)
         then
            Append_To (Constrained_List, New_Copy (Current_Parameter));
         else
            Append_To (Unconstrained_List, New_Copy (Current_Parameter));
         end if;

         Next (Current_Parameter);
      end loop;

      --  Unconstrained parameters are returned first

      Append_List_To (Unconstrained_List, Constrained_List);

      return Unconstrained_List;
   end Build_Ordered_Parameters_List;

   ----------------------------------
   -- Build_Passive_Partition_Stub --
   ----------------------------------

   procedure Build_Passive_Partition_Stub (U : Node_Id) is
      Pkg_Spec : Node_Id;
      Pkg_Name : String_Id;
      L        : List_Id;
      Reg      : Node_Id;
      Loc      : constant Source_Ptr := Sloc (U);

   begin
      --  Verify that the implementation supports distribution, by accessing
      --  a type defined in the proper version of system.rpc

      declare
         Dist_OK : Entity_Id;
         pragma Warnings (Off, Dist_OK);
      begin
         Dist_OK := RTE (RE_Params_Stream_Type);
      end;

      --  Use body if present, spec otherwise

      if Nkind (U) = N_Package_Declaration then
         Pkg_Spec := Specification (U);
         L := Visible_Declarations (Pkg_Spec);
      else
         Pkg_Spec := Parent (Corresponding_Spec (U));
         L := Declarations (U);
      end if;

      Get_Library_Unit_Name_String (Pkg_Spec);
      Pkg_Name := String_From_Name_Buffer;
      Reg :=
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Register_Passive_Package), Loc),
          Parameter_Associations => New_List (
            Make_String_Literal (Loc, Pkg_Name),
            Make_Attribute_Reference (Loc,
              Prefix         =>
                New_Occurrence_Of (Defining_Entity (Pkg_Spec), Loc),
              Attribute_Name =>
                Name_Version)));
      Append_To (L, Reg);
      Analyze (Reg);
   end Build_Passive_Partition_Stub;

   --------------------------------------
   -- Build_RPC_Receiver_Specification --
   --------------------------------------

   function Build_RPC_Receiver_Specification
     (RPC_Receiver      : Entity_Id;
      Request_Parameter : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (RPC_Receiver);
   begin
      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => RPC_Receiver,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Request_Parameter,
              Parameter_Type      =>
                New_Occurrence_Of (RTE (RE_Request_Access), Loc))));
   end Build_RPC_Receiver_Specification;

   ----------------------------------------
   -- Build_Remote_Subprogram_Proxy_Type --
   ----------------------------------------

   function Build_Remote_Subprogram_Proxy_Type
     (Loc            : Source_Ptr;
      ACR_Expression : Node_Id) return Node_Id
   is
   begin
      return
        Make_Record_Definition (Loc,
          Tagged_Present  => True,
          Limited_Present => True,
          Component_List  =>
            Make_Component_List (Loc,

              Component_Items => New_List (
                Make_Component_Declaration (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc,
                      Name_All_Calls_Remote),
                  Component_Definition =>
                    Make_Component_Definition (Loc,
                      Subtype_Indication =>
                        New_Occurrence_Of (Standard_Boolean, Loc)),
                  Expression =>
                    ACR_Expression),

                Make_Component_Declaration (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc,
                      Name_Receiver),
                  Component_Definition =>
                    Make_Component_Definition (Loc,
                      Subtype_Indication =>
                        New_Occurrence_Of (RTE (RE_Address), Loc)),
                  Expression =>
                    New_Occurrence_Of (RTE (RE_Null_Address), Loc)),

                Make_Component_Declaration (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc,
                      Name_Subp_Id),
                  Component_Definition =>
                    Make_Component_Definition (Loc,
                      Subtype_Indication =>
                        New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc))))));
   end Build_Remote_Subprogram_Proxy_Type;

   ------------------------------------
   -- Build_Subprogram_Calling_Stubs --
   ------------------------------------

   function Build_Subprogram_Calling_Stubs
     (Vis_Decl                 : Node_Id;
      Subp_Id                  : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Locator                  : Entity_Id := Empty;
      New_Name                 : Name_Id   := No_Name) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Vis_Decl);

      Decls      : constant List_Id := New_List;
      Statements : constant List_Id := New_List;

      Subp_Spec : Node_Id;
      --  The specification of the body

      Controlling_Parameter : Entity_Id := Empty;

      Asynchronous_Expr : Node_Id := Empty;

      RCI_Locator : Entity_Id;

      Spec_To_Use : Node_Id;

      procedure Insert_Partition_Check (Parameter : Node_Id);
      --  Check that the parameter has been elaborated on the same partition
      --  than the controlling parameter (E.4(19)).

      ----------------------------
      -- Insert_Partition_Check --
      ----------------------------

      procedure Insert_Partition_Check (Parameter : Node_Id) is
         Parameter_Entity : constant Entity_Id :=
                              Defining_Identifier (Parameter);
      begin
         --  The expression that will be built is of the form:

         --    if not Same_Partition (Parameter, Controlling_Parameter) then
         --      raise Constraint_Error;
         --    end if;

         --  We do not check that Parameter is in Stub_Type since such a check
         --  has been inserted at the point of call already (a tag check since
         --  we have multiple controlling operands).

         Append_To (Decls,
           Make_Raise_Constraint_Error (Loc,
             Condition       =>
               Make_Op_Not (Loc,
                 Right_Opnd =>
                   Make_Function_Call (Loc,
                     Name =>
                       New_Occurrence_Of (RTE (RE_Same_Partition), Loc),
                     Parameter_Associations =>
                       New_List (
                         Unchecked_Convert_To (RTE (RE_RACW_Stub_Type_Access),
                           New_Occurrence_Of (Parameter_Entity, Loc)),
                         Unchecked_Convert_To (RTE (RE_RACW_Stub_Type_Access),
                           New_Occurrence_Of (Controlling_Parameter, Loc))))),
             Reason => CE_Partition_Check_Failed));
      end Insert_Partition_Check;

   --  Start of processing for Build_Subprogram_Calling_Stubs

   begin
      Subp_Spec := Copy_Specification (Loc,
        Spec     => Specification (Vis_Decl),
        New_Name => New_Name);

      if Locator = Empty then
         RCI_Locator := RCI_Cache;
         Spec_To_Use := Specification (Vis_Decl);
      else
         RCI_Locator := Locator;
         Spec_To_Use := Subp_Spec;
      end if;

      --  Find a controlling argument if we have a stub type. Also check
      --  if this subprogram can be made asynchronous.

      if Present (Stub_Type)
         and then Present (Parameter_Specifications (Spec_To_Use))
      then
         declare
            Current_Parameter : Node_Id :=
                                  First (Parameter_Specifications
                                           (Spec_To_Use));
         begin
            while Present (Current_Parameter) loop
               if
                 Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type)
               then
                  if Controlling_Parameter = Empty then
                     Controlling_Parameter :=
                       Defining_Identifier (Current_Parameter);
                  else
                     Insert_Partition_Check (Current_Parameter);
                  end if;
               end if;

               Next (Current_Parameter);
            end loop;
         end;
      end if;

      pragma Assert (No (Stub_Type) or else Present (Controlling_Parameter));

      if Dynamically_Asynchronous then
         Asynchronous_Expr := Make_Selected_Component (Loc,
                                Prefix        => Controlling_Parameter,
                                Selector_Name => Name_Asynchronous);
      end if;

      Specific_Build_General_Calling_Stubs
        (Decls                 => Decls,
         Statements            => Statements,
         Target                => Specific_Build_Stub_Target (Loc,
                                    Decls, RCI_Locator, Controlling_Parameter),
         Subprogram_Id         => Subp_Id,
         Asynchronous          => Asynchronous_Expr,
         Is_Known_Asynchronous => Asynchronous
                                    and then not Dynamically_Asynchronous,
         Is_Known_Non_Asynchronous
                               => not Asynchronous
                                    and then not Dynamically_Asynchronous,
         Is_Function           => Nkind (Spec_To_Use) =
                                    N_Function_Specification,
         Spec                  => Spec_To_Use,
         Stub_Type             => Stub_Type,
         RACW_Type             => RACW_Type,
         Nod                   => Vis_Decl);

      RCI_Calling_Stubs_Table.Set
        (Defining_Unit_Name (Specification (Vis_Decl)),
         Defining_Unit_Name (Spec_To_Use));

      return
        Make_Subprogram_Body (Loc,
          Specification              => Subp_Spec,
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements));
   end Build_Subprogram_Calling_Stubs;

   -------------------------
   -- Build_Subprogram_Id --
   -------------------------

   function Build_Subprogram_Id
     (Loc : Source_Ptr;
      E   : Entity_Id) return Node_Id
   is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            return Make_String_Literal  (Loc, Get_Subprogram_Id (E));
         when others =>
            return Make_Integer_Literal (Loc, Get_Subprogram_Id (E));
      end case;
   end Build_Subprogram_Id;

   ------------------------
   -- Copy_Specification --
   ------------------------

   function Copy_Specification
     (Loc         : Source_Ptr;
      Spec        : Node_Id;
      Object_Type : Entity_Id := Empty;
      Stub_Type   : Entity_Id := Empty;
      New_Name    : Name_Id   := No_Name) return Node_Id
   is
      Parameters : List_Id := No_List;

      Current_Parameter  : Node_Id;
      Current_Identifier : Entity_Id;
      Current_Type       : Node_Id;
      Current_Etype      : Entity_Id;

      Name_For_New_Spec : Name_Id;

      New_Identifier : Entity_Id;

   --  Comments needed in body below ???

   begin
      if New_Name = No_Name then
         pragma Assert (Nkind (Spec) = N_Function_Specification
                or else Nkind (Spec) = N_Procedure_Specification);

         Name_For_New_Spec := Chars (Defining_Unit_Name (Spec));
      else
         Name_For_New_Spec := New_Name;
      end if;

      if Present (Parameter_Specifications (Spec)) then
         Parameters        := New_List;
         Current_Parameter := First (Parameter_Specifications (Spec));
         while Present (Current_Parameter) loop
            Current_Identifier := Defining_Identifier (Current_Parameter);
            Current_Type       := Parameter_Type (Current_Parameter);

            if Nkind (Current_Type) = N_Access_Definition then
               Current_Etype := Entity (Subtype_Mark (Current_Type));

               if Present (Object_Type) then
                  pragma Assert (
                    Root_Type (Current_Etype) = Root_Type (Object_Type));
                  Current_Type :=
                    Make_Access_Definition (Loc,
                      Subtype_Mark => New_Occurrence_Of (Stub_Type, Loc));
               else
                  Current_Type :=
                    Make_Access_Definition (Loc,
                      Subtype_Mark =>
                        New_Occurrence_Of (Current_Etype, Loc));
               end if;

            else
               Current_Etype := Entity (Current_Type);

               if Present (Object_Type)
                 and then Current_Etype = Object_Type
               then
                  Current_Type := New_Occurrence_Of (Stub_Type, Loc);
               else
                  Current_Type := New_Occurrence_Of (Current_Etype, Loc);
               end if;
            end if;

            New_Identifier := Make_Defining_Identifier (Loc,
              Chars (Current_Identifier));

            Append_To (Parameters,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => New_Identifier,
                Parameter_Type      => Current_Type,
                In_Present          => In_Present (Current_Parameter),
                Out_Present         => Out_Present (Current_Parameter),
                Expression          =>
                  New_Copy_Tree (Expression (Current_Parameter))));

            --  For a regular formal parameter (that needs to be marshalled
            --  in the context of remote calls), set the Etype now, because
            --  marshalling processing might need it.

            if Is_Entity_Name (Current_Type) then
               Set_Etype (New_Identifier, Entity (Current_Type));

            --  Current_Type is an access definition, special processing
            --  (not requiring etype) will occur for marshalling.

            else
               null;
            end if;

            Next (Current_Parameter);
         end loop;
      end if;

      case Nkind (Spec) is

         when N_Function_Specification | N_Access_Function_Definition =>
            return
              Make_Function_Specification (Loc,
                Defining_Unit_Name       =>
                  Make_Defining_Identifier (Loc,
                    Chars => Name_For_New_Spec),
                Parameter_Specifications => Parameters,
                Subtype_Mark             =>
                  New_Occurrence_Of (Entity (Subtype_Mark (Spec)), Loc));

         when N_Procedure_Specification | N_Access_Procedure_Definition =>
            return
              Make_Procedure_Specification (Loc,
                Defining_Unit_Name       =>
                  Make_Defining_Identifier (Loc,
                    Chars => Name_For_New_Spec),
                Parameter_Specifications => Parameters);

         when others =>
            raise Program_Error;
      end case;
   end Copy_Specification;

   ---------------------------
   -- Could_Be_Asynchronous --
   ---------------------------

   function Could_Be_Asynchronous (Spec : Node_Id) return Boolean is
      Current_Parameter : Node_Id;

   begin
      if Present (Parameter_Specifications (Spec)) then
         Current_Parameter := First (Parameter_Specifications (Spec));
         while Present (Current_Parameter) loop
            if Out_Present (Current_Parameter) then
               return False;
            end if;

            Next (Current_Parameter);
         end loop;
      end if;

      return True;
   end Could_Be_Asynchronous;

   ---------------------------
   -- Declare_Create_NVList --
   ---------------------------

   procedure Declare_Create_NVList
     (Loc    : Source_Ptr;
      NVList : Entity_Id;
      Decls  : List_Id;
      Stmts  : List_Id)
   is
   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => NVList,
          Aliased_Present     => False,
          Object_Definition   =>
              New_Occurrence_Of (RTE (RE_NVList_Ref), Loc)));

      Append_To (Stmts,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_NVList_Create), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (NVList, Loc))));
   end Declare_Create_NVList;

   ---------------------------------------------
   -- Expand_All_Calls_Remote_Subprogram_Call --
   ---------------------------------------------

   procedure Expand_All_Calls_Remote_Subprogram_Call (N : Node_Id) is
      Called_Subprogram : constant Entity_Id  := Entity (Name (N));
      RCI_Package       : constant Entity_Id  := Scope (Called_Subprogram);
      Loc               : constant Source_Ptr := Sloc (N);
      RCI_Locator       : Node_Id;
      RCI_Cache         : Entity_Id;
      Calling_Stubs     : Node_Id;
      E_Calling_Stubs   : Entity_Id;

   begin
      E_Calling_Stubs := RCI_Calling_Stubs_Table.Get (Called_Subprogram);

      if E_Calling_Stubs = Empty then
         RCI_Cache := RCI_Locator_Table.Get (RCI_Package);

         if RCI_Cache = Empty then
            RCI_Locator :=
              RCI_Package_Locator
                (Loc, Specification (Unit_Declaration_Node (RCI_Package)));
            Prepend_To (Current_Sem_Unit_Declarations, RCI_Locator);

            --  The RCI_Locator package is inserted at the top level in the
            --  current unit, and must appear in the proper scope, so that it
            --  is not prematurely removed by the GCC back-end.

            declare
               Scop : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);

            begin
               if Ekind (Scop) = E_Package_Body then
                  New_Scope (Spec_Entity (Scop));

               elsif Ekind (Scop) = E_Subprogram_Body then
                  New_Scope
                     (Corresponding_Spec (Unit_Declaration_Node (Scop)));

               else
                  New_Scope (Scop);
               end if;

               Analyze (RCI_Locator);
               Pop_Scope;
            end;

            RCI_Cache   := Defining_Unit_Name (RCI_Locator);

         else
            RCI_Locator := Parent (RCI_Cache);
         end if;

         Calling_Stubs := Build_Subprogram_Calling_Stubs
           (Vis_Decl               => Parent (Parent (Called_Subprogram)),
            Subp_Id                =>
              Build_Subprogram_Id (Loc, Called_Subprogram),
            Asynchronous           => Nkind (N) = N_Procedure_Call_Statement
                                        and then
                                      Is_Asynchronous (Called_Subprogram),
            Locator                => RCI_Cache,
            New_Name               => New_Internal_Name ('S'));
         Insert_After (RCI_Locator, Calling_Stubs);
         Analyze (Calling_Stubs);
         E_Calling_Stubs := Defining_Unit_Name (Specification (Calling_Stubs));
      end if;

      Rewrite (Name (N), New_Occurrence_Of (E_Calling_Stubs, Loc));
   end Expand_All_Calls_Remote_Subprogram_Call;

   ---------------------------------
   -- Expand_Calling_Stubs_Bodies --
   ---------------------------------

   procedure Expand_Calling_Stubs_Bodies (Unit_Node : Node_Id) is
      Spec  : constant Node_Id := Specification (Unit_Node);
      Decls : constant List_Id := Visible_Declarations (Spec);
   begin
      New_Scope (Scope_Of_Spec (Spec));
      Add_Calling_Stubs_To_Declarations
        (Specification (Unit_Node), Decls);
      Pop_Scope;
   end Expand_Calling_Stubs_Bodies;

   -----------------------------------
   -- Expand_Receiving_Stubs_Bodies --
   -----------------------------------

   procedure Expand_Receiving_Stubs_Bodies (Unit_Node : Node_Id) is
      Spec  : Node_Id;
      Decls : List_Id;
      Temp  : List_Id;

   begin
      if Nkind (Unit_Node) = N_Package_Declaration then
         Spec  := Specification (Unit_Node);
         Decls := Private_Declarations (Spec);

         if No (Decls) then
            Decls := Visible_Declarations (Spec);
         end if;

         New_Scope (Scope_Of_Spec (Spec));
         Specific_Add_Receiving_Stubs_To_Declarations (Spec, Decls);

      else
         Spec  :=
           Package_Specification_Of_Scope (Corresponding_Spec (Unit_Node));
         Decls := Declarations (Unit_Node);
         New_Scope (Scope_Of_Spec (Unit_Node));
         Temp := New_List;
         Specific_Add_Receiving_Stubs_To_Declarations (Spec, Temp);
         Insert_List_Before (First (Decls), Temp);
      end if;

      Pop_Scope;
   end Expand_Receiving_Stubs_Bodies;

   --------------------
   -- GARLIC_Support --
   --------------------

   package body GARLIC_Support is

      --  Local subprograms

      procedure Add_RACW_Read_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id);
      --  Add Read attribute in Decls for the RACW type. The Read attribute
      --  is added right after the RACW_Type declaration while the body is
      --  inserted after Declarations.

      procedure Add_RACW_Write_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         RPC_Receiver     : Node_Id;
         Declarations     : List_Id);
      --  Same thing for the Write attribute

      function Stream_Parameter return Node_Id;
      function Result return Node_Id;
      function Object return Node_Id renames Result;
      --  Functions to create occurrences of the formal parameter names of
      --  the 'Read and 'Write attributes.

      Loc : Source_Ptr;
      --  Shared source location used by Add_{Read,Write}_Read_Attribute
      --  and their ancillary subroutines (set on entry by Add_RACW_Features).

      procedure Add_RAS_Access_TSS (N : Node_Id);
      --  Add a subprogram body for RAS Access TSS

      -------------------------------------
      -- Add_Obj_RPC_Receiver_Completion --
      -------------------------------------

      procedure Add_Obj_RPC_Receiver_Completion
        (Loc           : Source_Ptr;
         Decls         : List_Id;
         RPC_Receiver  : Entity_Id;
         Stub_Elements : Stub_Structure) is
      begin
         --  The RPC receiver body should not be the completion of the
         --  declaration recorded in the stub structure, because then the
         --  occurrences of the formal parameters within the body should
         --  refer to the entities from the declaration, not from the
         --  completion, to which we do not have easy access. Instead, the
         --  RPC receiver body acts as its own declaration, and the RPC
         --  receiver declaration is completed by a renaming-as-body.

         Append_To (Decls,
           Make_Subprogram_Renaming_Declaration (Loc,
             Specification =>
               Copy_Specification (Loc,
                 Specification (Stub_Elements.RPC_Receiver_Decl)),
             Name          => New_Occurrence_Of (RPC_Receiver, Loc)));
      end Add_Obj_RPC_Receiver_Completion;

      -----------------------
      -- Add_RACW_Features --
      -----------------------

      procedure Add_RACW_Features
        (RACW_Type         : Entity_Id;
         Stub_Type         : Entity_Id;
         Stub_Type_Access  : Entity_Id;
         RPC_Receiver_Decl : Node_Id;
         Declarations      : List_Id)
      is
         RPC_Receiver : Node_Id;
         Is_RAS       : constant Boolean := not Comes_From_Source (RACW_Type);

      begin
         Loc := Sloc (RACW_Type);

         if Is_RAS then

            --  For a RAS, the RPC receiver is that of the RCI unit,
            --  not that of the corresponding distributed object type.
            --  We retrieve its address from the local proxy object.

            RPC_Receiver := Make_Selected_Component (Loc,
              Prefix         =>
                Unchecked_Convert_To (RTE (RE_RAS_Proxy_Type_Access), Object),
              Selector_Name  => Make_Identifier (Loc, Name_Receiver));

         else
            RPC_Receiver := Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (
                Defining_Unit_Name (Specification (RPC_Receiver_Decl)), Loc),
              Attribute_Name => Name_Address);
         end if;

         Add_RACW_Write_Attribute (
           RACW_Type,
           Stub_Type,
           Stub_Type_Access,
           RPC_Receiver,
           Declarations);

         Add_RACW_Read_Attribute (
           RACW_Type,
           Stub_Type,
           Stub_Type_Access,
           Declarations);
      end Add_RACW_Features;

      -----------------------------
      -- Add_RACW_Read_Attribute --
      -----------------------------

      procedure Add_RACW_Read_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id)
      is
         Proc_Decl : Node_Id;
         Attr_Decl : Node_Id;

         Body_Node : Node_Id;

         Decls             : List_Id;
         Statements        : List_Id;
         Local_Statements  : List_Id;
         Remote_Statements : List_Id;
         --  Various parts of the procedure

         Procedure_Name    : constant Name_Id   :=
                               New_Internal_Name ('R');
         Source_Partition  : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('P'));
         Source_Receiver   : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('S'));
         Source_Address    : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('P'));
         Local_Stub        : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('L'));
         Stubbed_Result    : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('S'));
         Asynchronous_Flag : constant Entity_Id :=
                               Asynchronous_Flags_Table.Get (RACW_Type);
         pragma Assert (Present (Asynchronous_Flag));

      --  Start of processing for Add_RACW_Read_Attribute

      begin
         --  Generate object declarations

         Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Source_Partition,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Partition_ID), Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Source_Receiver,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Source_Address,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Local_Stub,
             Aliased_Present     => True,
             Object_Definition   => New_Occurrence_Of (Stub_Type, Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Stubbed_Result,
             Object_Definition   =>
               New_Occurrence_Of (Stub_Type_Access, Loc),
             Expression          =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Local_Stub, Loc),
                 Attribute_Name =>
                   Name_Unchecked_Access)));

         --  Read the source Partition_ID and RPC_Receiver from incoming stream

         Statements := New_List (
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (RTE (RE_Partition_ID), Loc),
             Attribute_Name => Name_Read,
             Expressions    => New_List (
               Stream_Parameter,
               New_Occurrence_Of (Source_Partition, Loc))),

           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc),
             Attribute_Name =>
               Name_Read,
             Expressions    => New_List (
               Stream_Parameter,
               New_Occurrence_Of (Source_Receiver, Loc))),

           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc),
             Attribute_Name =>
               Name_Read,
             Expressions    => New_List (
               Stream_Parameter,
               New_Occurrence_Of (Source_Address, Loc))));

         --  Build_Get_Unique_RP_Call needs the type of Stubbed_Result

         Set_Etype (Stubbed_Result, Stub_Type_Access);

         --  If the Address is Null_Address, then return a null object

         Append_To (Statements,
           Make_Implicit_If_Statement (RACW_Type,
             Condition       =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Occurrence_Of (Source_Address, Loc),
                 Right_Opnd => Make_Integer_Literal (Loc, Uint_0)),
             Then_Statements => New_List (
               Make_Assignment_Statement (Loc,
                 Name       => Result,
                 Expression => Make_Null (Loc)),
               Make_Return_Statement (Loc))));

         --  If the RACW denotes an object created on the current partition,
         --  Local_Statements will be executed. The real object will be used.

         Local_Statements := New_List (
           Make_Assignment_Statement (Loc,
             Name       => Result,
             Expression =>
               Unchecked_Convert_To (RACW_Type,
                 OK_Convert_To (RTE (RE_Address),
                   New_Occurrence_Of (Source_Address, Loc)))));

         --  If the object is located on another partition, then a stub object
         --  will be created with all the information needed to rebuild the
         --  real object at the other end.

         Remote_Statements := New_List (

           Make_Assignment_Statement (Loc,
             Name       => Make_Selected_Component (Loc,
               Prefix        => Stubbed_Result,
               Selector_Name => Name_Origin),
             Expression =>
               New_Occurrence_Of (Source_Partition, Loc)),

           Make_Assignment_Statement (Loc,
             Name       => Make_Selected_Component (Loc,
               Prefix        => Stubbed_Result,
               Selector_Name => Name_Receiver),
             Expression =>
               New_Occurrence_Of (Source_Receiver, Loc)),

           Make_Assignment_Statement (Loc,
             Name       => Make_Selected_Component (Loc,
               Prefix        => Stubbed_Result,
               Selector_Name => Name_Addr),
             Expression =>
               New_Occurrence_Of (Source_Address, Loc)));

         Append_To (Remote_Statements,
           Make_Assignment_Statement (Loc,
             Name       => Make_Selected_Component (Loc,
               Prefix        => Stubbed_Result,
               Selector_Name => Name_Asynchronous),
             Expression =>
               New_Occurrence_Of (Asynchronous_Flag, Loc)));

         Append_List_To (Remote_Statements,
           Build_Get_Unique_RP_Call (Loc, Stubbed_Result, Stub_Type));
         --  ??? Issue with asynchronous calls here: the Asynchronous
         --  flag is set on the stub type if, and only if, the RACW type
         --  has a pragma Asynchronous. This is incorrect for RACWs that
         --  implement RAS types, because in that case the /designated
         --  subprogram/ (not the type) might be asynchronous, and
         --  that causes the stub to need to be asynchronous too.
         --  A solution is to transport a RAS as a struct containing
         --  a RACW and an asynchronous flag, and to properly alter
         --  the Asynchronous component in the stub type in the RAS's
         --  Input TSS.

         Append_To (Remote_Statements,
           Make_Assignment_Statement (Loc,
             Name       => Result,
             Expression => Unchecked_Convert_To (RACW_Type,
               New_Occurrence_Of (Stubbed_Result, Loc))));

         --  Distinguish between the local and remote cases, and execute the
         --  appropriate piece of code.

         Append_To (Statements,
           Make_Implicit_If_Statement (RACW_Type,
             Condition       =>
               Make_Op_Eq (Loc,
                 Left_Opnd  =>
                   Make_Function_Call (Loc,
                     Name => New_Occurrence_Of (
                       RTE (RE_Get_Local_Partition_Id), Loc)),
                 Right_Opnd => New_Occurrence_Of (Source_Partition, Loc)),
             Then_Statements => Local_Statements,
             Else_Statements => Remote_Statements));

         Build_Stream_Procedure
           (Loc, RACW_Type, Body_Node,
            Make_Defining_Identifier (Loc, Procedure_Name),
            Statements, Outp => True);
         Set_Declarations (Body_Node, Decls);

         Proc_Decl := Make_Subprogram_Declaration (Loc,
           Copy_Specification (Loc, Specification (Body_Node)));

         Attr_Decl :=
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Occurrence_Of (RACW_Type, Loc),
             Chars      => Name_Read,
             Expression =>
               New_Occurrence_Of (
                 Defining_Unit_Name (Specification (Proc_Decl)), Loc));

         Insert_After (Declaration_Node (RACW_Type), Proc_Decl);
         Insert_After (Proc_Decl, Attr_Decl);
         Append_To (Declarations, Body_Node);
      end Add_RACW_Read_Attribute;

      ------------------------------
      -- Add_RACW_Write_Attribute --
      ------------------------------

      procedure Add_RACW_Write_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         RPC_Receiver     : Node_Id;
         Declarations     : List_Id)
      is
         Body_Node : Node_Id;
         Proc_Decl : Node_Id;
         Attr_Decl : Node_Id;

         Statements        : List_Id;
         Local_Statements  : List_Id;
         Remote_Statements : List_Id;
         Null_Statements   : List_Id;

         Procedure_Name : constant Name_Id := New_Internal_Name ('R');

      begin
         --  Build the code fragment corresponding to the marshalling of a
         --  local object.

         Local_Statements := New_List (

           Pack_Entity_Into_Stream_Access (Loc,
             Stream => Stream_Parameter,
             Object => RTE (RE_Get_Local_Partition_Id)),

           Pack_Node_Into_Stream_Access (Loc,
             Stream => Stream_Parameter,
             Object => OK_Convert_To (RTE (RE_Unsigned_64), RPC_Receiver),
             Etyp   => RTE (RE_Unsigned_64)),

          Pack_Node_Into_Stream_Access (Loc,
            Stream => Stream_Parameter,
            Object => OK_Convert_To (RTE (RE_Unsigned_64),
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  Make_Explicit_Dereference (Loc,
                    Prefix => Object),
                Attribute_Name => Name_Address)),
            Etyp   => RTE (RE_Unsigned_64)));

         --  Build the code fragment corresponding to the marshalling of
         --  a remote object.

         Remote_Statements := New_List (

           Pack_Node_Into_Stream_Access (Loc,
            Stream => Stream_Parameter,
            Object =>
               Make_Selected_Component (Loc,
                 Prefix        => Unchecked_Convert_To (Stub_Type_Access,
                   Object),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Origin)),
            Etyp   => RTE (RE_Partition_ID)),

           Pack_Node_Into_Stream_Access (Loc,
            Stream => Stream_Parameter,
            Object =>
               Make_Selected_Component (Loc,
                 Prefix        => Unchecked_Convert_To (Stub_Type_Access,
                   Object),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Receiver)),
            Etyp   => RTE (RE_Unsigned_64)),

           Pack_Node_Into_Stream_Access (Loc,
            Stream => Stream_Parameter,
            Object =>
               Make_Selected_Component (Loc,
                 Prefix        => Unchecked_Convert_To (Stub_Type_Access,
                   Object),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Addr)),
            Etyp   => RTE (RE_Unsigned_64)));

         --  Build code fragment corresponding to marshalling of a null object

         Null_Statements := New_List (

           Pack_Entity_Into_Stream_Access (Loc,
             Stream => Stream_Parameter,
             Object => RTE (RE_Get_Local_Partition_Id)),

           Pack_Node_Into_Stream_Access (Loc,
             Stream => Stream_Parameter,
             Object => OK_Convert_To (RTE (RE_Unsigned_64), RPC_Receiver),
             Etyp   => RTE (RE_Unsigned_64)),

           Pack_Node_Into_Stream_Access (Loc,
             Stream => Stream_Parameter,
             Object => Make_Integer_Literal (Loc, Uint_0),
             Etyp   => RTE (RE_Unsigned_64)));

         Statements := New_List (
           Make_Implicit_If_Statement (RACW_Type,
             Condition       =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => Object,
                 Right_Opnd => Make_Null (Loc)),
             Then_Statements => Null_Statements,
             Elsif_Parts     => New_List (
               Make_Elsif_Part (Loc,
                 Condition       =>
                   Make_Op_Eq (Loc,
                     Left_Opnd  =>
                       Make_Attribute_Reference (Loc,
                         Prefix         => Object,
                         Attribute_Name => Name_Tag),
                     Right_Opnd =>
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Stub_Type, Loc),
                         Attribute_Name => Name_Tag)),
                 Then_Statements => Remote_Statements)),
             Else_Statements => Local_Statements));

         Build_Stream_Procedure
           (Loc, RACW_Type, Body_Node,
            Make_Defining_Identifier (Loc, Procedure_Name),
            Statements, Outp => False);

         Proc_Decl := Make_Subprogram_Declaration (Loc,
           Copy_Specification (Loc, Specification (Body_Node)));

         Attr_Decl :=
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Occurrence_Of (RACW_Type, Loc),
             Chars      => Name_Write,
             Expression =>
               New_Occurrence_Of (
                 Defining_Unit_Name (Specification (Proc_Decl)), Loc));

         Insert_After (Declaration_Node (RACW_Type), Proc_Decl);
         Insert_After (Proc_Decl, Attr_Decl);
         Append_To (Declarations, Body_Node);
      end Add_RACW_Write_Attribute;

      ------------------------
      -- Add_RAS_Access_TSS --
      ------------------------

      procedure Add_RAS_Access_TSS (N : Node_Id) is
         Loc : constant Source_Ptr := Sloc (N);

         Ras_Type : constant Entity_Id := Defining_Identifier (N);
         Fat_Type : constant Entity_Id := Equivalent_Type (Ras_Type);
         --  Ras_Type is the access to subprogram type while Fat_Type is the
         --  corresponding record type.

         RACW_Type : constant Entity_Id :=
                       Underlying_RACW_Type (Ras_Type);
         Desig     : constant Entity_Id :=
                       Etype (Designated_Type (RACW_Type));

         Stub_Elements : constant Stub_Structure :=
                           Stubs_Table.Get (Desig);
         pragma Assert (Stub_Elements /= Empty_Stub_Structure);

         Proc : constant Entity_Id :=
                  Make_Defining_Identifier (Loc,
                    Chars => Make_TSS_Name (Ras_Type, TSS_RAS_Access));

         Proc_Spec : Node_Id;

         --  Formal parameters

         Package_Name : constant Entity_Id :=
                          Make_Defining_Identifier (Loc,
                            Chars => Name_P);
         --  Target package

         Subp_Id : constant Entity_Id :=
                     Make_Defining_Identifier (Loc,
                       Chars => Name_S);
         --  Target subprogram

         Asynch_P : constant Entity_Id :=
                      Make_Defining_Identifier (Loc,
                        Chars => Name_Asynchronous);
         --  Is the procedure to which the 'Access applies asynchronous?

         All_Calls_Remote : constant Entity_Id :=
                              Make_Defining_Identifier (Loc,
                                Chars => Name_All_Calls_Remote);
         --  True if an All_Calls_Remote pragma applies to the RCI unit
         --  that contains the subprogram.

         --  Common local variables

         Proc_Decls      : List_Id;
         Proc_Statements : List_Id;

         Origin : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_Internal_Name ('P'));

         --  Additional local variables for the local case

         Proxy_Addr : constant Entity_Id :=
                        Make_Defining_Identifier (Loc,
                          Chars => New_Internal_Name ('P'));

         --  Additional local variables for the remote case

         Local_Stub : constant Entity_Id :=
                        Make_Defining_Identifier (Loc,
                          Chars => New_Internal_Name ('L'));

         Stub_Ptr : constant Entity_Id :=
                      Make_Defining_Identifier (Loc,
                        Chars => New_Internal_Name ('S'));

         function Set_Field
           (Field_Name : Name_Id;
            Value      : Node_Id) return Node_Id;
         --  Construct an assignment that sets the named component in the
         --  returned record

         ---------------
         -- Set_Field --
         ---------------

         function Set_Field
           (Field_Name : Name_Id;
            Value      : Node_Id) return Node_Id
         is
         begin
            return
              Make_Assignment_Statement (Loc,
                Name       =>
                  Make_Selected_Component (Loc,
                    Prefix        => Stub_Ptr,
                    Selector_Name => Field_Name),
                Expression => Value);
         end Set_Field;

      --  Start of processing for Add_RAS_Access_TSS

      begin
         Proc_Decls := New_List (

         --  Common declarations

           Make_Object_Declaration (Loc,
             Defining_Identifier => Origin,
             Constant_Present    => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Partition_ID), Loc),
             Expression          =>
               Make_Function_Call (Loc,
                 Name                   =>
                   New_Occurrence_Of (RTE (RE_Get_Active_Partition_Id), Loc),
                 Parameter_Associations => New_List (
                   New_Occurrence_Of (Package_Name, Loc)))),

         --  Declaration use only in the local case: proxy address

           Make_Object_Declaration (Loc,
             Defining_Identifier => Proxy_Addr,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc)),

         --  Declarations used only in the remote case: stub object and
         --  stub pointer.

           Make_Object_Declaration (Loc,
             Defining_Identifier => Local_Stub,
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (Stub_Elements.Stub_Type, Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Stub_Ptr,
             Object_Definition   =>
               New_Occurrence_Of (Stub_Elements.Stub_Type_Access, Loc),
             Expression          =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Local_Stub, Loc),
                 Attribute_Name => Name_Unchecked_Access)));

         Set_Etype (Stub_Ptr, Stub_Elements.Stub_Type_Access);
         --  Build_Get_Unique_RP_Call needs this information

         --  Note: Here we assume that the Fat_Type is a record
         --  containing just a pointer to a proxy or stub object.

         Proc_Statements := New_List (

         --  Generate:

         --    Get_RAS_Info (Pkg, Subp, PA);
         --    if Origin = Local_Partition_Id
         --      and then not All_Calls_Remote
         --    then
         --       return Fat_Type!(PA);
         --    end if;

            Make_Procedure_Call_Statement (Loc,
              Name =>
                New_Occurrence_Of (RTE (RE_Get_RAS_Info), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Package_Name, Loc),
                New_Occurrence_Of (Subp_Id, Loc),
                New_Occurrence_Of (Proxy_Addr, Loc))),

           Make_Implicit_If_Statement (N,
             Condition =>
               Make_And_Then (Loc,
                 Left_Opnd  =>
                   Make_Op_Eq (Loc,
                     Left_Opnd =>
                       New_Occurrence_Of (Origin, Loc),
                     Right_Opnd =>
                       Make_Function_Call (Loc,
                         New_Occurrence_Of (
                           RTE (RE_Get_Local_Partition_Id), Loc))),
                 Right_Opnd =>
                   Make_Op_Not (Loc,
                     New_Occurrence_Of (All_Calls_Remote, Loc))),
             Then_Statements => New_List (
               Make_Return_Statement (Loc,
                 Unchecked_Convert_To (Fat_Type,
                   OK_Convert_To (RTE (RE_Address),
                     New_Occurrence_Of (Proxy_Addr, Loc)))))),

           Set_Field (Name_Origin,
               New_Occurrence_Of (Origin, Loc)),

           Set_Field (Name_Receiver,
             Make_Function_Call (Loc,
               Name                   =>
                 New_Occurrence_Of (RTE (RE_Get_RCI_Package_Receiver), Loc),
               Parameter_Associations => New_List (
                 New_Occurrence_Of (Package_Name, Loc)))),

           Set_Field (Name_Addr, New_Occurrence_Of (Proxy_Addr, Loc)),

         --  E.4.1(9) A remote call is asynchronous if it is a call to
         --  a procedure, or a call through a value of an access-to-procedure
         --  type, to which a pragma Asynchronous applies.

         --    Parameter Asynch_P is true when the procedure is asynchronous;
         --    Expression Asynch_T is true when the type is asynchronous.

           Set_Field (Name_Asynchronous,
             Make_Or_Else (Loc,
               New_Occurrence_Of (Asynch_P, Loc),
               New_Occurrence_Of (Boolean_Literals (
                 Is_Asynchronous (Ras_Type)), Loc))));

         Append_List_To (Proc_Statements,
           Build_Get_Unique_RP_Call
             (Loc, Stub_Ptr, Stub_Elements.Stub_Type));

         --  Return the newly created value

         Append_To (Proc_Statements,
           Make_Return_Statement (Loc,
             Expression =>
               Unchecked_Convert_To (Fat_Type,
                 New_Occurrence_Of (Stub_Ptr, Loc))));

         Proc_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Proc,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Package_Name,
                 Parameter_Type      =>
                   New_Occurrence_Of (Standard_String, Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Subp_Id,
                 Parameter_Type      =>
                   New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Asynch_P,
                 Parameter_Type      =>
                   New_Occurrence_Of (Standard_Boolean, Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => All_Calls_Remote,
                 Parameter_Type      =>
                   New_Occurrence_Of (Standard_Boolean, Loc))),

            Subtype_Mark =>
              New_Occurrence_Of (Fat_Type, Loc));

         --  Set the kind and return type of the function to prevent
         --  ambiguities between Ras_Type and Fat_Type in subsequent analysis.

         Set_Ekind (Proc, E_Function);
         Set_Etype (Proc, Fat_Type);

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification              => Proc_Spec,
             Declarations               => Proc_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Proc_Statements)));

         Set_TSS (Fat_Type, Proc);
      end Add_RAS_Access_TSS;

      -----------------------
      -- Add_RAST_Features --
      -----------------------

      procedure Add_RAST_Features
        (Vis_Decl : Node_Id;
         RAS_Type : Entity_Id)
      is
         pragma Warnings (Off);
         pragma Unreferenced (RAS_Type);
         pragma Warnings (On);
      begin
         Add_RAS_Access_TSS (Vis_Decl);
      end Add_RAST_Features;

      -----------------------------------------
      -- Add_Receiving_Stubs_To_Declarations --
      -----------------------------------------

      procedure Add_Receiving_Stubs_To_Declarations
        (Pkg_Spec : Node_Id;
         Decls    : List_Id)
      is
         Loc : constant Source_Ptr := Sloc (Pkg_Spec);

         Request_Parameter : Node_Id;

         Pkg_RPC_Receiver            : constant Entity_Id :=
                                         Make_Defining_Identifier (Loc,
                                           New_Internal_Name ('H'));
         Pkg_RPC_Receiver_Statements : List_Id;
         Pkg_RPC_Receiver_Cases      : constant List_Id := New_List;
         Pkg_RPC_Receiver_Body       : Node_Id;
         --  A Pkg_RPC_Receiver is built to decode the request

         Lookup_RAS_Info : constant Entity_Id :=
                             Make_Defining_Identifier (Loc,
                               Chars => New_Internal_Name ('R'));
         --  A remote subprogram is created to allow peers to look up
         --  RAS information using subprogram ids.

         Subp_Id    : Entity_Id;
         Subp_Index : Entity_Id;
         --  Subprogram_Id as read from the incoming stream

         Current_Declaration       : Node_Id;
         Current_Subprogram_Number : Int := First_RCI_Subprogram_Id;
         Current_Stubs             : Node_Id;

         Subp_Info_Array : constant Entity_Id :=
                             Make_Defining_Identifier (Loc,
                               Chars => New_Internal_Name ('I'));

         Subp_Info_List : constant List_Id := New_List;

         Register_Pkg_Actuals : constant List_Id := New_List;

         All_Calls_Remote_E  : Entity_Id;
         Proxy_Object_Addr   : Entity_Id;

         procedure Append_Stubs_To
           (RPC_Receiver_Cases : List_Id;
            Stubs              : Node_Id;
            Subprogram_Number  : Int);
         --  Add one case to the specified RPC receiver case list
         --  associating Subprogram_Number with the subprogram declared
         --  by Declaration, for which we have receiving stubs in Stubs.

         ---------------------
         -- Append_Stubs_To --
         ---------------------

         procedure Append_Stubs_To
           (RPC_Receiver_Cases : List_Id;
            Stubs              : Node_Id;
            Subprogram_Number  : Int)
         is
         begin
            Append_To (RPC_Receiver_Cases,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices =>
                   New_List (Make_Integer_Literal (Loc, Subprogram_Number)),
                Statements       =>
                  New_List (
                    Make_Procedure_Call_Statement (Loc,
                      Name                   =>
                        New_Occurrence_Of (
                          Defining_Entity (Stubs), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Request_Parameter, Loc))))));
         end Append_Stubs_To;

      --  Start of processing for Add_Receiving_Stubs_To_Declarations

      begin
         --  Building receiving stubs consist in several operations:

         --    - a package RPC receiver must be built. This subprogram
         --      will get a Subprogram_Id from the incoming stream
         --      and will dispatch the call to the right subprogram

         --    - a receiving stub for any subprogram visible in the package
         --      spec. This stub will read all the parameters from the stream,
         --      and put the result as well as the exception occurrence in the
         --      output stream

         --    - a dummy package with an empty spec and a body made of an
         --      elaboration part, whose job is to register the receiving
         --      part of this RCI package on the name server. This is done
         --      by calling System.Partition_Interface.Register_Receiving_Stub

         Build_RPC_Receiver_Body (
           RPC_Receiver => Pkg_RPC_Receiver,
           Request      => Request_Parameter,
           Subp_Id      => Subp_Id,
           Subp_Index   => Subp_Index,
           Stmts        => Pkg_RPC_Receiver_Statements,
           Decl         => Pkg_RPC_Receiver_Body);
         pragma Assert (Subp_Id = Subp_Index);

         --  A null subp_id denotes a call through a RAS, in which case the
         --  next Uint_64 element in the stream is the address of the local
         --  proxy object, from which we can retrieve the actual subprogram id.

         Append_To (Pkg_RPC_Receiver_Statements,
           Make_Implicit_If_Statement (Pkg_Spec,
             Condition =>
               Make_Op_Eq (Loc,
                 New_Occurrence_Of (Subp_Id, Loc),
                 Make_Integer_Literal (Loc, 0)),
             Then_Statements => New_List (
               Make_Assignment_Statement (Loc,
                 Name =>
                   New_Occurrence_Of (Subp_Id, Loc),
                 Expression =>
                   Make_Selected_Component (Loc,
                     Prefix =>
                       Unchecked_Convert_To (RTE (RE_RAS_Proxy_Type_Access),
                         OK_Convert_To (RTE (RE_Address),
                           Make_Attribute_Reference (Loc,
                             Prefix =>
                               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc),
                             Attribute_Name =>
                               Name_Input,
                             Expressions => New_List (
                               Make_Selected_Component (Loc,
                                 Prefix        => Request_Parameter,
                                 Selector_Name => Name_Params))))),
                     Selector_Name =>
                       Make_Identifier (Loc, Name_Subp_Id))))));

         --  Build a subprogram for RAS information lookups

         Current_Declaration :=
           Make_Subprogram_Declaration (Loc,
             Specification =>
               Make_Function_Specification (Loc,
                 Defining_Unit_Name =>
                   Lookup_RAS_Info,
                 Parameter_Specifications => New_List (
                   Make_Parameter_Specification (Loc,
                     Defining_Identifier =>
                       Make_Defining_Identifier (Loc, Name_Subp_Id),
                     In_Present =>
                       True,
                     Parameter_Type =>
                       New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc))),
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Unsigned_64), Loc)));
         Append_To (Decls, Current_Declaration);
         Analyze (Current_Declaration);

         Current_Stubs := Build_Subprogram_Receiving_Stubs
           (Vis_Decl     => Current_Declaration,
            Asynchronous => False);
         Append_To (Decls, Current_Stubs);
         Analyze (Current_Stubs);

         Append_Stubs_To (Pkg_RPC_Receiver_Cases,
           Stubs       =>
             Current_Stubs,
           Subprogram_Number => 1);

         --  For each subprogram, the receiving stub will be built and a
         --  case statement will be made on the Subprogram_Id to dispatch
         --  to the right subprogram.

         All_Calls_Remote_E := Boolean_Literals (
           Has_All_Calls_Remote (Defining_Entity (Pkg_Spec)));

         Overload_Counter_Table.Reset;

         Current_Declaration := First (Visible_Declarations (Pkg_Spec));
         while Present (Current_Declaration) loop
            if Nkind (Current_Declaration) = N_Subprogram_Declaration
              and then Comes_From_Source (Current_Declaration)
            then
               declare
                  Loc : constant Source_Ptr :=
                          Sloc (Current_Declaration);
                  --  While specifically processing Current_Declaration, use
                  --  its Sloc as the location of all generated nodes.

                  Subp_Def : constant Entity_Id :=
                               Defining_Unit_Name
                                 (Specification (Current_Declaration));

                  Subp_Val : String_Id;

               begin
                  pragma Assert (Current_Subprogram_Number =
                    Get_Subprogram_Id (Subp_Def));

                  --  Build receiving stub

                  Current_Stubs :=
                    Build_Subprogram_Receiving_Stubs
                      (Vis_Decl     => Current_Declaration,
                       Asynchronous =>
                         Nkind (Specification (Current_Declaration)) =
                             N_Procedure_Specification
                           and then Is_Asynchronous (Subp_Def));

                  Append_To (Decls, Current_Stubs);
                  Analyze (Current_Stubs);

                  --  Build RAS proxy

                  Add_RAS_Proxy_And_Analyze (Decls,
                    Vis_Decl           =>
                      Current_Declaration,
                    All_Calls_Remote_E =>
                      All_Calls_Remote_E,
                    Proxy_Object_Addr  =>
                      Proxy_Object_Addr);

                  --  Compute distribution identifier

                  Assign_Subprogram_Identifier (
                    Subp_Def,
                    Current_Subprogram_Number,
                    Subp_Val);

                  --  Add subprogram descriptor (RCI_Subp_Info) to the
                  --  subprograms table for this receiver. The aggregate
                  --  below must be kept consistent with the declaration
                  --  of type RCI_Subp_Info in System.Partition_Interface.

                  Append_To (Subp_Info_List,
                    Make_Component_Association (Loc,
                      Choices => New_List (
                        Make_Integer_Literal (Loc,
                          Current_Subprogram_Number)),
                      Expression =>
                        Make_Aggregate (Loc,
                          Component_Associations => New_List (
                            Make_Component_Association (Loc,
                              Choices => New_List (
                                Make_Identifier (Loc, Name_Addr)),
                              Expression =>
                                New_Occurrence_Of (
                                  Proxy_Object_Addr, Loc))))));

                  Append_Stubs_To (Pkg_RPC_Receiver_Cases,
                    Stubs =>
                      Current_Stubs,
                    Subprogram_Number =>
                      Current_Subprogram_Number);
               end;

               Current_Subprogram_Number := Current_Subprogram_Number + 1;
            end if;

            Next (Current_Declaration);
         end loop;

         --  If we receive an invalid Subprogram_Id, it is best to do nothing
         --  rather than raising an exception since we do not want someone
         --  to crash a remote partition by sending invalid subprogram ids.
         --  This is consistent with the other parts of the case statement
         --  since even in presence of incorrect parameters in the stream,
         --  every exception will be caught and (if the subprogram is not an
         --  APC) put into the result stream and sent away.

         Append_To (Pkg_RPC_Receiver_Cases,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices =>
               New_List (Make_Others_Choice (Loc)),
             Statements       =>
               New_List (Make_Null_Statement (Loc))));

         Append_To (Pkg_RPC_Receiver_Statements,
           Make_Case_Statement (Loc,
             Expression   =>
               New_Occurrence_Of (Subp_Id, Loc),
             Alternatives => Pkg_RPC_Receiver_Cases));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Info_Array,
             Constant_Present    => True,
             Aliased_Present     => True,
             Object_Definition   =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_RCI_Subp_Info_Array), Loc),
                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     New_List (
                       Make_Range (Loc,
                         Low_Bound  => Make_Integer_Literal (Loc,
                           First_RCI_Subprogram_Id),
                         High_Bound =>
                           Make_Integer_Literal (Loc,
                             First_RCI_Subprogram_Id
                             + List_Length (Subp_Info_List) - 1))))),
             Expression          =>
               Make_Aggregate (Loc,
                 Component_Associations => Subp_Info_List)));
         Analyze (Last (Decls));

         Append_To (Decls,
           Make_Subprogram_Body (Loc,
             Specification =>
               Copy_Specification (Loc, Parent (Lookup_RAS_Info)),
             Declarations =>
               No_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Return_Statement (Loc,
                     Expression => OK_Convert_To (RTE (RE_Unsigned_64),
                       Make_Selected_Component (Loc,
                         Prefix =>
                           Make_Indexed_Component (Loc,
                             Prefix =>
                               New_Occurrence_Of (Subp_Info_Array, Loc),
                             Expressions => New_List (
                               Convert_To (Standard_Integer,
                                 Make_Identifier (Loc, Name_Subp_Id)))),
                         Selector_Name =>
                           Make_Identifier (Loc, Name_Addr))))))));
         Analyze (Last (Decls));

         Append_To (Decls, Pkg_RPC_Receiver_Body);
         Analyze (Last (Decls));

         Get_Library_Unit_Name_String (Pkg_Spec);
         Append_To (Register_Pkg_Actuals,
            --  Name
           Make_String_Literal (Loc,
             Strval => String_From_Name_Buffer));

         Append_To (Register_Pkg_Actuals,
            --  Receiver
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Pkg_RPC_Receiver, Loc),
             Attribute_Name =>
               Name_Unrestricted_Access));

         Append_To (Register_Pkg_Actuals,
            --  Version
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Defining_Entity (Pkg_Spec), Loc),
             Attribute_Name =>
               Name_Version));

         Append_To (Register_Pkg_Actuals,
            --  Subp_Info
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Subp_Info_Array, Loc),
             Attribute_Name =>
               Name_Address));

         Append_To (Register_Pkg_Actuals,
            --  Subp_Info_Len
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Subp_Info_Array, Loc),
             Attribute_Name =>
               Name_Length));

         Append_To (Decls,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Register_Receiving_Stub), Loc),
             Parameter_Associations => Register_Pkg_Actuals));
         Analyze (Last (Decls));
      end Add_Receiving_Stubs_To_Declarations;

      ---------------------------------
      -- Build_General_Calling_Stubs --
      ---------------------------------

      procedure Build_General_Calling_Stubs
        (Decls                     : List_Id;
         Statements                : List_Id;
         Target_Partition          : Entity_Id;
         Target_RPC_Receiver       : Node_Id;
         Subprogram_Id             : Node_Id;
         Asynchronous              : Node_Id   := Empty;
         Is_Known_Asynchronous     : Boolean   := False;
         Is_Known_Non_Asynchronous : Boolean   := False;
         Is_Function               : Boolean;
         Spec                      : Node_Id;
         Stub_Type                 : Entity_Id := Empty;
         RACW_Type                 : Entity_Id := Empty;
         Nod                       : Node_Id)
      is
         Loc : constant Source_Ptr := Sloc (Nod);

         Stream_Parameter : Node_Id;
         --  Name of the stream used to transmit parameters to the
         --  remote package.

         Result_Parameter : Node_Id;
         --  Name of the result parameter (in non-APC cases) which get the
         --  result of the remote subprogram.

         Exception_Return_Parameter : Node_Id;
         --  Name of the parameter which will hold the exception sent by the
         --  remote subprogram.

         Current_Parameter : Node_Id;
         --  Current parameter being handled

         Ordered_Parameters_List : constant List_Id :=
                                     Build_Ordered_Parameters_List (Spec);

         Asynchronous_Statements     : List_Id := No_List;
         Non_Asynchronous_Statements : List_Id := No_List;
         --  Statements specifics to the Asynchronous/Non-Asynchronous cases

         Extra_Formal_Statements : constant List_Id := New_List;
         --  List of statements for extra formal parameters. It will appear
         --  after the regular statements for writing out parameters.

         pragma Warnings (Off);
         pragma Unreferenced (RACW_Type);
         --  Used only for the PolyORB case
         pragma Warnings (On);

      begin
         --  The general form of a calling stub for a given subprogram is:

         --    procedure X (...) is P : constant Partition_ID :=
         --      RCI_Cache.Get_Active_Partition_ID; Stream, Result : aliased
         --      System.RPC.Params_Stream_Type (0); begin
         --       Put_Package_RPC_Receiver_In_Stream; (the package RPC receiver
         --                  comes from RCI_Cache.Get_RCI_Package_Receiver)
         --       Put_Subprogram_Id_In_Stream; Put_Parameters_In_Stream; Do_RPC
         --       (Stream, Result); Read_Exception_Occurrence_From_Result;
         --       Raise_It;
         --       Read_Out_Parameters_And_Function_Return_From_Stream; end X;

         --  There are some variations: Do_APC is called for an asynchronous
         --  procedure and the part after the call is completely ommitted as
         --  well as the declaration of Result. For a function call, 'Input is
         --  always used to read the result even if it is constrained.

         Stream_Parameter :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('S'));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Stream_Parameter,
             Aliased_Present     => True,
             Object_Definition   =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc),
                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints =>
                       New_List (Make_Integer_Literal (Loc, 0))))));

         if not Is_Known_Asynchronous then
            Result_Parameter :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Result_Parameter,
                Aliased_Present     => True,
                Object_Definition   =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc),
                    Constraint   =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints =>
                          New_List (Make_Integer_Literal (Loc, 0))))));

            Exception_Return_Parameter :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Exception_Return_Parameter,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc)));

         else
            Result_Parameter := Empty;
            Exception_Return_Parameter := Empty;
         end if;

         --  Put first the RPC receiver corresponding to the remote package

         Append_To (Statements,
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc),
             Attribute_Name => Name_Write,
             Expressions    => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of (Stream_Parameter, Loc),
                 Attribute_Name =>
                   Name_Access),
               Target_RPC_Receiver)));

         --  Then put the Subprogram_Id of the subprogram we want to call in
         --  the stream.

         Append_To (Statements,
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc),
             Attribute_Name =>
               Name_Write,
             Expressions      => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of (Stream_Parameter, Loc),
                 Attribute_Name => Name_Access),
               Subprogram_Id)));

         Current_Parameter := First (Ordered_Parameters_List);
         while Present (Current_Parameter) loop
            declare
               Typ             : constant Node_Id :=
                                   Parameter_Type (Current_Parameter);
               Etyp            : Entity_Id;
               Constrained     : Boolean;
               Value           : Node_Id;
               Extra_Parameter : Entity_Id;

            begin
               if Is_RACW_Controlling_Formal
                 (Current_Parameter, Stub_Type)
               then
                  --  In the case of a controlling formal argument, we marshall
                  --  its addr field rather than the local stub.

                  Append_To (Statements,
                     Pack_Node_Into_Stream (Loc,
                       Stream => Stream_Parameter,
                       Object =>
                         Make_Selected_Component (Loc,
                           Prefix        =>
                             Defining_Identifier (Current_Parameter),
                           Selector_Name => Name_Addr),
                       Etyp   => RTE (RE_Unsigned_64)));

               else
                  Value := New_Occurrence_Of
                    (Defining_Identifier (Current_Parameter), Loc);

                  --  Access type parameters are transmitted as in out
                  --  parameters. However, a dereference is needed so that
                  --  we marshall the designated object.

                  if Nkind (Typ) = N_Access_Definition then
                     Value := Make_Explicit_Dereference (Loc, Value);
                     Etyp  := Etype (Subtype_Mark (Typ));
                  else
                     Etyp := Etype (Typ);
                  end if;

                  Constrained :=
                    Is_Constrained (Etyp) or else Is_Elementary_Type (Etyp);

                  --  Any parameter but unconstrained out parameters are
                  --  transmitted to the peer.

                  if In_Present (Current_Parameter)
                    or else not Out_Present (Current_Parameter)
                    or else not Constrained
                  then
                     Append_To (Statements,
                       Make_Attribute_Reference (Loc,
                         Prefix         =>
                           New_Occurrence_Of (Etyp, Loc),
                         Attribute_Name =>
                           Output_From_Constrained (Constrained),
                         Expressions    => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix         =>
                               New_Occurrence_Of (Stream_Parameter, Loc),
                             Attribute_Name => Name_Access),
                           Value)));
                  end if;
               end if;

               --  If the current parameter has a dynamic constrained status,
               --  then this status is transmitted as well.
               --  This should be done for accessibility as well ???

               if Nkind (Typ) /= N_Access_Definition
                 and then Need_Extra_Constrained (Current_Parameter)
               then
                  --  In this block, we do not use the extra formal that has
                  --  been created because it does not exist at the time of
                  --  expansion when building calling stubs for remote access
                  --  to subprogram types. We create an extra variable of this
                  --  type and push it in the stream after the regular
                  --  parameters.

                  Extra_Parameter := Make_Defining_Identifier
                                       (Loc, New_Internal_Name ('P'));

                  Append_To (Decls,
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Extra_Parameter,
                       Constant_Present    => True,
                       Object_Definition   =>
                          New_Occurrence_Of (Standard_Boolean, Loc),
                       Expression          =>
                          Make_Attribute_Reference (Loc,
                            Prefix         =>
                              New_Occurrence_Of (
                                Defining_Identifier (Current_Parameter), Loc),
                            Attribute_Name => Name_Constrained)));

                  Append_To (Extra_Formal_Statements,
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         New_Occurrence_Of (Standard_Boolean, Loc),
                       Attribute_Name =>
                         Name_Write,
                       Expressions    => New_List (
                         Make_Attribute_Reference (Loc,
                           Prefix         =>
                             New_Occurrence_Of (Stream_Parameter, Loc),
                           Attribute_Name =>
                             Name_Access),
                         New_Occurrence_Of (Extra_Parameter, Loc))));
               end if;

               Next (Current_Parameter);
            end;
         end loop;

         --  Append the formal statements list to the statements

         Append_List_To (Statements, Extra_Formal_Statements);

         if not Is_Known_Non_Asynchronous then

            --  Build the call to System.RPC.Do_APC

            Asynchronous_Statements := New_List (
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Do_Apc), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Target_Partition, Loc),
                  Make_Attribute_Reference (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Stream_Parameter, Loc),
                    Attribute_Name =>
                      Name_Access))));
         else
            Asynchronous_Statements := No_List;
         end if;

         if not Is_Known_Asynchronous then

            --  Build the call to System.RPC.Do_RPC

            Non_Asynchronous_Statements := New_List (
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Do_Rpc), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Target_Partition, Loc),

                  Make_Attribute_Reference (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Stream_Parameter, Loc),
                    Attribute_Name =>
                      Name_Access),

                  Make_Attribute_Reference (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Result_Parameter, Loc),
                    Attribute_Name =>
                      Name_Access))));

            --  Read the exception occurrence from the result stream and
            --  reraise it. It does no harm if this is a Null_Occurrence since
            --  this does nothing.

            Append_To (Non_Asynchronous_Statements,
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),

                Attribute_Name =>
                  Name_Read,

                Expressions    => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Result_Parameter, Loc),
                    Attribute_Name =>
                      Name_Access),
                  New_Occurrence_Of (Exception_Return_Parameter, Loc))));

            Append_To (Non_Asynchronous_Statements,
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Reraise_Occurrence), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Exception_Return_Parameter, Loc))));

            if Is_Function then

               --  If this is a function call, then read the value and return
               --  it. The return value is written/read using 'Output/'Input.

               Append_To (Non_Asynchronous_Statements,
                 Make_Tag_Check (Loc,
                   Make_Return_Statement (Loc,
                     Expression =>
                       Make_Attribute_Reference (Loc,
                         Prefix         =>
                           New_Occurrence_Of (
                             Etype (Subtype_Mark (Spec)), Loc),

                         Attribute_Name => Name_Input,

                         Expressions    => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix         =>
                               New_Occurrence_Of (Result_Parameter, Loc),
                             Attribute_Name => Name_Access))))));

            else
               --  Loop around parameters and assign out (or in out)
               --  parameters. In the case of RACW, controlling arguments
               --  cannot possibly have changed since they are remote, so we do
               --  not read them from the stream.

               Current_Parameter := First (Ordered_Parameters_List);
               while Present (Current_Parameter) loop
                  declare
                     Typ   : constant Node_Id :=
                               Parameter_Type (Current_Parameter);
                     Etyp  : Entity_Id;
                     Value : Node_Id;

                  begin
                     Value :=
                       New_Occurrence_Of
                         (Defining_Identifier (Current_Parameter), Loc);

                     if Nkind (Typ) = N_Access_Definition then
                        Value := Make_Explicit_Dereference (Loc, Value);
                        Etyp  := Etype (Subtype_Mark (Typ));
                     else
                        Etyp := Etype (Typ);
                     end if;

                     if (Out_Present (Current_Parameter)
                          or else Nkind (Typ) = N_Access_Definition)
                       and then Etyp /= Stub_Type
                     then
                        Append_To (Non_Asynchronous_Statements,
                           Make_Attribute_Reference (Loc,
                             Prefix         =>
                               New_Occurrence_Of (Etyp, Loc),

                             Attribute_Name => Name_Read,

                             Expressions    => New_List (
                               Make_Attribute_Reference (Loc,
                                 Prefix         =>
                                   New_Occurrence_Of (Result_Parameter, Loc),
                                 Attribute_Name =>
                                   Name_Access),
                               Value)));
                     end if;
                  end;

                  Next (Current_Parameter);
               end loop;
            end if;
         end if;

         if Is_Known_Asynchronous then
            Append_List_To (Statements, Asynchronous_Statements);

         elsif Is_Known_Non_Asynchronous then
            Append_List_To (Statements, Non_Asynchronous_Statements);

         else
            pragma Assert (Present (Asynchronous));
            Prepend_To (Asynchronous_Statements,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
                Attribute_Name => Name_Write,
                Expressions    => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Stream_Parameter, Loc),
                    Attribute_Name => Name_Access),
                  New_Occurrence_Of (Standard_True, Loc))));

            Prepend_To (Non_Asynchronous_Statements,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
                Attribute_Name => Name_Write,
                Expressions    => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Stream_Parameter, Loc),
                    Attribute_Name => Name_Access),
                  New_Occurrence_Of (Standard_False, Loc))));

            Append_To (Statements,
              Make_Implicit_If_Statement (Nod,
                Condition       => Asynchronous,
                Then_Statements => Asynchronous_Statements,
                Else_Statements => Non_Asynchronous_Statements));
         end if;
      end Build_General_Calling_Stubs;

      -----------------------------
      -- Build_RPC_Receiver_Body --
      -----------------------------

      procedure Build_RPC_Receiver_Body
        (RPC_Receiver : Entity_Id;
         Request      : out Entity_Id;
         Subp_Id      : out Entity_Id;
         Subp_Index   : out Entity_Id;
         Stmts        : out List_Id;
         Decl         : out Node_Id)
      is
         Loc : constant Source_Ptr := Sloc (RPC_Receiver);

         RPC_Receiver_Spec  : Node_Id;
         RPC_Receiver_Decls : List_Id;

      begin
         Request := Make_Defining_Identifier (Loc, Name_R);

         RPC_Receiver_Spec :=
           Build_RPC_Receiver_Specification
             (RPC_Receiver      => RPC_Receiver,
              Request_Parameter => Request);

         Subp_Id    := Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
         Subp_Index := Subp_Id;

         --  Subp_Id may not be a constant, because in the case of the RPC
         --  receiver for an RCI package, when a call is received from a RAS
         --  dereference, it will be assigned during subsequent processing.

         RPC_Receiver_Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Id,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc),
             Expression          =>
               Make_Attribute_Reference (Loc,
                 Prefix          =>
                   New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc),
                 Attribute_Name  => Name_Input,
                 Expressions     => New_List (
                                      Make_Selected_Component (Loc,
                                        Prefix        => Request,
                                        Selector_Name => Name_Params)))));

         Stmts := New_List;

         Decl :=
           Make_Subprogram_Body (Loc,
             Specification              => RPC_Receiver_Spec,
             Declarations               => RPC_Receiver_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stmts));
      end Build_RPC_Receiver_Body;

      -----------------------
      -- Build_Stub_Target --
      -----------------------

      function Build_Stub_Target
        (Loc                   : Source_Ptr;
         Decls                 : List_Id;
         RCI_Locator           : Entity_Id;
         Controlling_Parameter : Entity_Id) return RPC_Target
      is
         Target_Info : RPC_Target (PCS_Kind => Name_GARLIC_DSA);
      begin
         Target_Info.Partition :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
         if Present (Controlling_Parameter) then
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Target_Info.Partition,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Partition_ID), Loc),

                Expression          =>
                  Make_Selected_Component (Loc,
                    Prefix        => Controlling_Parameter,
                    Selector_Name => Name_Origin)));

            Target_Info.RPC_Receiver :=
              Make_Selected_Component (Loc,
                Prefix        => Controlling_Parameter,
                Selector_Name => Name_Receiver);

         else
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Target_Info.Partition,
                Constant_Present    => True,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Partition_ID), Loc),

                Expression          =>
                  Make_Function_Call (Loc,
                    Name => Make_Selected_Component (Loc,
                      Prefix        =>
                        Make_Identifier (Loc, Chars (RCI_Locator)),
                      Selector_Name =>
                        Make_Identifier (Loc,
                          Name_Get_Active_Partition_ID)))));

            Target_Info.RPC_Receiver :=
              Make_Selected_Component (Loc,
                Prefix        =>
                  Make_Identifier (Loc, Chars (RCI_Locator)),
                Selector_Name =>
                  Make_Identifier (Loc, Name_Get_RCI_Package_Receiver));
         end if;
         return Target_Info;
      end Build_Stub_Target;

      ---------------------
      -- Build_Stub_Type --
      ---------------------

      procedure Build_Stub_Type
        (RACW_Type         : Entity_Id;
         Stub_Type         : Entity_Id;
         Stub_Type_Decl    : out Node_Id;
         RPC_Receiver_Decl : out Node_Id)
      is
         Loc    : constant Source_Ptr := Sloc (Stub_Type);
         Is_RAS : constant Boolean    := not Comes_From_Source (RACW_Type);

      begin
         Stub_Type_Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Stub_Type,
             Type_Definition     =>
               Make_Record_Definition (Loc,
                 Tagged_Present  => True,
                 Limited_Present => True,
                 Component_List  =>
                   Make_Component_List (Loc,
                     Component_Items => New_List (

                       Make_Component_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Name_Origin),
                         Component_Definition =>
                           Make_Component_Definition (Loc,
                             Aliased_Present    => False,
                             Subtype_Indication =>
                               New_Occurrence_Of (
                                 RTE (RE_Partition_ID), Loc))),

                       Make_Component_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Name_Receiver),
                         Component_Definition =>
                           Make_Component_Definition (Loc,
                             Aliased_Present    => False,
                             Subtype_Indication =>
                               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc))),

                       Make_Component_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Name_Addr),
                         Component_Definition =>
                           Make_Component_Definition (Loc,
                             Aliased_Present    => False,
                             Subtype_Indication =>
                               New_Occurrence_Of (RTE (RE_Unsigned_64), Loc))),

                       Make_Component_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Name_Asynchronous),
                         Component_Definition =>
                           Make_Component_Definition (Loc,
                             Aliased_Present    => False,
                             Subtype_Indication =>
                               New_Occurrence_Of (
                                 Standard_Boolean, Loc)))))));

         if Is_RAS then
            RPC_Receiver_Decl := Empty;
         else
            declare
               RPC_Receiver_Request : constant Entity_Id :=
                                        Make_Defining_Identifier (Loc, Name_R);
            begin
               RPC_Receiver_Decl :=
                 Make_Subprogram_Declaration (Loc,
                   Build_RPC_Receiver_Specification (
                     RPC_Receiver      => Make_Defining_Identifier (Loc,
                                            New_Internal_Name ('R')),
                     Request_Parameter => RPC_Receiver_Request));
            end;
         end if;
      end Build_Stub_Type;

      --------------------------------------
      -- Build_Subprogram_Receiving_Stubs --
      --------------------------------------

      function Build_Subprogram_Receiving_Stubs
        (Vis_Decl                 : Node_Id;
         Asynchronous             : Boolean;
         Dynamically_Asynchronous : Boolean   := False;
         Stub_Type                : Entity_Id := Empty;
         RACW_Type                : Entity_Id := Empty;
         Parent_Primitive         : Entity_Id := Empty) return Node_Id
      is
         Loc : constant Source_Ptr := Sloc (Vis_Decl);

         Request_Parameter : Node_Id;
         --  ???

         Decls : constant List_Id := New_List;
         --  All the parameters will get declared before calling the real
         --  subprograms. Also the out parameters will be declared.

         Statements : constant List_Id := New_List;

         Extra_Formal_Statements : constant List_Id := New_List;
         --  Statements concerning extra formal parameters

         After_Statements : constant List_Id := New_List;
         --  Statements to be executed after the subprogram call

         Inner_Decls : List_Id := No_List;
         --  In case of a function, the inner declarations are needed since
         --  the result may be unconstrained.

         Excep_Handlers : List_Id := No_List;
         Excep_Choice   : Entity_Id;
         Excep_Code     : List_Id;

         Parameter_List : constant List_Id := New_List;
         --  List of parameters to be passed to the subprogram

         Current_Parameter : Node_Id;

         Ordered_Parameters_List : constant List_Id :=
                                     Build_Ordered_Parameters_List
                                       (Specification (Vis_Decl));

         Subp_Spec : Node_Id;
         --  Subprogram specification

         Called_Subprogram : Node_Id;
         --  The subprogram to call

         Null_Raise_Statement : Node_Id;

         Dynamic_Async : Entity_Id;

      begin
         if Present (RACW_Type) then
            Called_Subprogram :=
              New_Occurrence_Of (Parent_Primitive, Loc);
         else
            Called_Subprogram :=
              New_Occurrence_Of (
                Defining_Unit_Name (Specification (Vis_Decl)), Loc);
         end if;

         Request_Parameter :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         if Dynamically_Asynchronous then
            Dynamic_Async :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
         else
            Dynamic_Async := Empty;
         end if;

         if not Asynchronous or Dynamically_Asynchronous then

            --  The first statement after the subprogram call is a statement to
            --  writes a Null_Occurrence into the result stream.

            Null_Raise_Statement :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),
                Attribute_Name => Name_Write,
                Expressions    => New_List (
                                    Make_Selected_Component (Loc,
                                      Prefix        => Request_Parameter,
                                      Selector_Name => Name_Result),
                  New_Occurrence_Of (RTE (RE_Null_Occurrence), Loc)));

            if Dynamically_Asynchronous then
               Null_Raise_Statement :=
                 Make_Implicit_If_Statement (Vis_Decl,
                   Condition       =>
                     Make_Op_Not (Loc, New_Occurrence_Of (Dynamic_Async, Loc)),
                   Then_Statements => New_List (Null_Raise_Statement));
            end if;

            Append_To (After_Statements, Null_Raise_Statement);
         end if;

         --  Loop through every parameter and get its value from the stream. If
         --  the parameter is unconstrained, then the parameter is read using
         --  'Input at the point of declaration.

         Current_Parameter := First (Ordered_Parameters_List);
         while Present (Current_Parameter) loop
            declare
               Etyp        : Entity_Id;
               Constrained : Boolean;

               Object : constant Entity_Id :=
                          Make_Defining_Identifier (Loc,
                            New_Internal_Name ('P'));

               Expr : Node_Id   := Empty;

               Is_Controlling_Formal : constant Boolean :=
                                         Is_RACW_Controlling_Formal
                                           (Current_Parameter, Stub_Type);

            begin
               Set_Ekind (Object, E_Variable);

               if Is_Controlling_Formal then

                  --  We have a controlling formal parameter. Read its address
                  --  rather than a real object. The address is in Unsigned_64
                  --  form.

                  Etyp := RTE (RE_Unsigned_64);
               else
                  Etyp := Etype (Parameter_Type (Current_Parameter));
               end if;

               Constrained :=
                 Is_Constrained (Etyp) or else Is_Elementary_Type (Etyp);

               if In_Present (Current_Parameter)
                 or else not Out_Present (Current_Parameter)
                 or else not Constrained
                 or else Is_Controlling_Formal
               then
                  --  If an input parameter is contrained, then its reading is
                  --  deferred until the beginning of the subprogram body. If
                  --  it is unconstrained, then an expression is built for
                  --  the object declaration and the variable is set using
                  --  'Input instead of 'Read.

                  if Constrained and then not Is_Controlling_Formal then
                     Append_To (Statements,
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Etyp, Loc),
                         Attribute_Name => Name_Read,
                         Expressions    => New_List (
                           Make_Selected_Component (Loc,
                             Prefix        => Request_Parameter,
                             Selector_Name => Name_Params),
                           New_Occurrence_Of (Object, Loc))));

                  else
                     Expr := Input_With_Tag_Check (Loc,
                       Var_Type => Etyp,
                       Stream   => Make_Selected_Component (Loc,
                                     Prefix        => Request_Parameter,
                                     Selector_Name => Name_Params));
                     Append_To (Decls, Expr);
                     Expr := Make_Function_Call (Loc,
                       New_Occurrence_Of (Defining_Unit_Name
                         (Specification (Expr)), Loc));
                  end if;
               end if;

               --  If we do not have to output the current parameter, then it
               --  can well be flagged as constant. This may allow further
               --  optimizations done by the back end.

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Object,
                   Constant_Present    => not Constrained
                     and then not Out_Present (Current_Parameter),
                   Object_Definition   =>
                     New_Occurrence_Of (Etyp, Loc),
                   Expression          => Expr));

               --  An out parameter may be written back using a 'Write
               --  attribute instead of a 'Output because it has been
               --  constrained by the parameter given to the caller. Note that
               --  out controlling arguments in the case of a RACW are not put
               --  back in the stream because the pointer on them has not
               --  changed.

               if Out_Present (Current_Parameter)
                 and then
                   Etype (Parameter_Type (Current_Parameter)) /= Stub_Type
               then
                  Append_To (After_Statements,
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Etyp, Loc),
                      Attribute_Name => Name_Write,
                      Expressions    => New_List (
                        Make_Selected_Component (Loc,
                          Prefix        => Request_Parameter,
                          Selector_Name => Name_Result),
                        New_Occurrence_Of (Object, Loc))));
               end if;

               --  For RACW controlling formals, the Etyp of Object is always
               --  an RACW, even if the parameter is not of an anonymous access
               --  type. In such case, we need to dereference it at call time.

               if Is_Controlling_Formal then
                  if Nkind (Parameter_Type (Current_Parameter)) /=
                    N_Access_Definition
                  then
                     Append_To (Parameter_List,
                       Make_Parameter_Association (Loc,
                         Selector_Name             =>
                           New_Occurrence_Of (
                             Defining_Identifier (Current_Parameter), Loc),
                         Explicit_Actual_Parameter =>
                           Make_Explicit_Dereference (Loc,
                             Unchecked_Convert_To (RACW_Type,
                               OK_Convert_To (RTE (RE_Address),
                                 New_Occurrence_Of (Object, Loc))))));

                  else
                     Append_To (Parameter_List,
                       Make_Parameter_Association (Loc,
                         Selector_Name             =>
                           New_Occurrence_Of (
                             Defining_Identifier (Current_Parameter), Loc),
                         Explicit_Actual_Parameter =>
                           Unchecked_Convert_To (RACW_Type,
                             OK_Convert_To (RTE (RE_Address),
                               New_Occurrence_Of (Object, Loc)))));
                  end if;

               else
                  Append_To (Parameter_List,
                    Make_Parameter_Association (Loc,
                      Selector_Name             =>
                        New_Occurrence_Of (
                          Defining_Identifier (Current_Parameter), Loc),
                      Explicit_Actual_Parameter =>
                        New_Occurrence_Of (Object, Loc)));
               end if;

               --  If the current parameter needs an extra formal, then read it
               --  from the stream and set the corresponding semantic field in
               --  the variable. If the kind of the parameter identifier is
               --  E_Void, then this is a compiler generated parameter that
               --  doesn't need an extra constrained status.

               --  The case of Extra_Accessibility should also be handled ???

               if Nkind (Parameter_Type (Current_Parameter)) /=
                                                         N_Access_Definition
                 and then
                   Ekind (Defining_Identifier (Current_Parameter)) /= E_Void
                 and then
                   Present (Extra_Constrained
                     (Defining_Identifier (Current_Parameter)))
               then
                  declare
                     Extra_Parameter : constant Entity_Id :=
                                         Extra_Constrained
                                           (Defining_Identifier
                                             (Current_Parameter));

                     Formal_Entity : constant Entity_Id :=
                                       Make_Defining_Identifier
                                           (Loc, Chars (Extra_Parameter));

                     Formal_Type : constant Entity_Id :=
                                     Etype (Extra_Parameter);

                  begin
                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Formal_Entity,
                         Object_Definition   =>
                           New_Occurrence_Of (Formal_Type, Loc)));

                     Append_To (Extra_Formal_Statements,
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (
                                             Formal_Type, Loc),
                         Attribute_Name => Name_Read,
                         Expressions    => New_List (
                           Make_Selected_Component (Loc,
                             Prefix        => Request_Parameter,
                             Selector_Name => Name_Params),
                           New_Occurrence_Of (Formal_Entity, Loc))));
                     Set_Extra_Constrained (Object, Formal_Entity);
                  end;
               end if;
            end;

            Next (Current_Parameter);
         end loop;

         --  Append the formal statements list at the end of regular statements

         Append_List_To (Statements, Extra_Formal_Statements);

         if Nkind (Specification (Vis_Decl)) = N_Function_Specification then

            --  The remote subprogram is a function. We build an inner block to
            --  be able to hold a potentially unconstrained result in a
            --  variable.

            declare
               Etyp   : constant Entity_Id :=
                          Etype (Subtype_Mark (Specification (Vis_Decl)));
               Result : constant Node_Id   :=
                          Make_Defining_Identifier (Loc,
                             New_Internal_Name ('R'));
            begin
               Inner_Decls := New_List (
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Result,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Etyp, Loc),
                   Expression          =>
                     Make_Function_Call (Loc,
                       Name                   => Called_Subprogram,
                       Parameter_Associations => Parameter_List)));

               Append_To (After_Statements,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Etyp, Loc),
                   Attribute_Name => Name_Output,
                   Expressions    => New_List (
                     Make_Selected_Component (Loc,
                       Prefix        => Request_Parameter,
                       Selector_Name => Name_Result),
                     New_Occurrence_Of (Result, Loc))));
            end;

            Append_To (Statements,
              Make_Block_Statement (Loc,
                Declarations               => Inner_Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => After_Statements)));

         else
            --  The remote subprogram is a procedure. We do not need any inner
            --  block in this case.

            if Dynamically_Asynchronous then
               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Dynamic_Async,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Boolean, Loc)));

               Append_To (Statements,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
                   Attribute_Name => Name_Read,
                   Expressions    => New_List (
                     Make_Selected_Component (Loc,
                       Prefix        => Request_Parameter,
                       Selector_Name => Name_Params),
                     New_Occurrence_Of (Dynamic_Async, Loc))));
            end if;

            Append_To (Statements,
              Make_Procedure_Call_Statement (Loc,
                Name                   => Called_Subprogram,
                Parameter_Associations => Parameter_List));

            Append_List_To (Statements, After_Statements);
         end if;

         if Asynchronous and then not Dynamically_Asynchronous then

            --  For an asynchronous procedure, add a null exception handler

            Excep_Handlers := New_List (
              Make_Exception_Handler (Loc,
                Exception_Choices => New_List (Make_Others_Choice (Loc)),
                Statements        => New_List (Make_Null_Statement (Loc))));

         else
            --  In the other cases, if an exception is raised, then the
            --  exception occurrence is copied into the output stream and
            --  no other output parameter is written.

            Excep_Choice :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

            Excep_Code := New_List (
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),
                Attribute_Name => Name_Write,
                Expressions    => New_List (
                                    Make_Selected_Component (Loc,
                                      Prefix        => Request_Parameter,
                                      Selector_Name => Name_Result),
                                    New_Occurrence_Of (Excep_Choice, Loc))));

            if Dynamically_Asynchronous then
               Excep_Code := New_List (
                 Make_Implicit_If_Statement (Vis_Decl,
                   Condition       => Make_Op_Not (Loc,
                     New_Occurrence_Of (Dynamic_Async, Loc)),
                   Then_Statements => Excep_Code));
            end if;

            Excep_Handlers := New_List (
              Make_Exception_Handler (Loc,
                Choice_Parameter   => Excep_Choice,
                Exception_Choices  => New_List (Make_Others_Choice (Loc)),
                Statements         => Excep_Code));

         end if;

         Subp_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       =>
               Make_Defining_Identifier (Loc, New_Internal_Name ('F')),

             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Request_Parameter,
                 Parameter_Type      =>
                   New_Occurrence_Of (RTE (RE_Request_Access), Loc))));

         return
           Make_Subprogram_Body (Loc,
             Specification              => Subp_Spec,
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements         => Statements,
                 Exception_Handlers => Excep_Handlers));
      end Build_Subprogram_Receiving_Stubs;

      ------------
      -- Result --
      ------------

      function Result return Node_Id is
      begin
         return Make_Identifier (Loc, Name_V);
      end Result;

      ----------------------
      -- Stream_Parameter --
      ----------------------

      function Stream_Parameter return Node_Id is
      begin
         return Make_Identifier (Loc, Name_S);
      end Stream_Parameter;

   end GARLIC_Support;

   -----------------------------
   -- Make_Selected_Component --
   -----------------------------

   function Make_Selected_Component
     (Loc           : Source_Ptr;
      Prefix        : Entity_Id;
      Selector_Name : Name_Id) return Node_Id
   is
   begin
      return Make_Selected_Component (Loc,
               Prefix        => New_Occurrence_Of (Prefix, Loc),
               Selector_Name => Make_Identifier (Loc, Selector_Name));
   end Make_Selected_Component;

   -----------------------
   -- Get_Subprogram_Id --
   -----------------------

   function Get_Subprogram_Id (Def : Entity_Id) return String_Id is
   begin
      return Get_Subprogram_Ids (Def).Str_Identifier;
   end Get_Subprogram_Id;

   -----------------------
   -- Get_Subprogram_Id --
   -----------------------

   function Get_Subprogram_Id (Def : Entity_Id) return Int is
   begin
      return Get_Subprogram_Ids (Def).Int_Identifier;
   end Get_Subprogram_Id;

   ------------------------
   -- Get_Subprogram_Ids --
   ------------------------

   function Get_Subprogram_Ids
     (Def : Entity_Id) return Subprogram_Identifiers
   is
      Result : Subprogram_Identifiers :=
                 Subprogram_Identifier_Table.Get (Def);

      Current_Declaration : Node_Id;
      Current_Subp        : Entity_Id;
      Current_Subp_Str    : String_Id;
      Current_Subp_Number : Int := First_RCI_Subprogram_Id;

   begin
      if Result.Str_Identifier = No_String then

         --  We are looking up this subprogram's identifier outside of the
         --  context of generating calling or receiving stubs. Hence we are
         --  processing an 'Access attribute_reference for an RCI subprogram,
         --  for the purpose of obtaining a RAS value.

         pragma Assert
           (Is_Remote_Call_Interface (Scope (Def))
              and then
               (Nkind (Parent (Def)) = N_Procedure_Specification
                  or else
                Nkind (Parent (Def)) = N_Function_Specification));

         Current_Declaration :=
           First (Visible_Declarations
             (Package_Specification_Of_Scope (Scope (Def))));
         while Present (Current_Declaration) loop
            if Nkind (Current_Declaration) = N_Subprogram_Declaration
              and then Comes_From_Source (Current_Declaration)
            then
               Current_Subp := Defining_Unit_Name (Specification (
                 Current_Declaration));
               Assign_Subprogram_Identifier
                 (Current_Subp, Current_Subp_Number, Current_Subp_Str);

               if Current_Subp = Def then
                  Result := (Current_Subp_Str, Current_Subp_Number);
               end if;

               Current_Subp_Number := Current_Subp_Number + 1;
            end if;

            Next (Current_Declaration);
         end loop;
      end if;

      pragma Assert (Result.Str_Identifier /= No_String);
      return Result;
   end Get_Subprogram_Ids;

   ----------
   -- Hash --
   ----------

   function Hash (F : Entity_Id) return Hash_Index is
   begin
      return Hash_Index (Natural (F) mod Positive (Hash_Index'Last + 1));
   end Hash;

   function Hash (F : Name_Id) return Hash_Index is
   begin
      return Hash_Index (Natural (F) mod Positive (Hash_Index'Last + 1));
   end Hash;

   --------------------------
   -- Input_With_Tag_Check --
   --------------------------

   function Input_With_Tag_Check
     (Loc      : Source_Ptr;
      Var_Type : Entity_Id;
      Stream   : Node_Id) return Node_Id
   is
   begin
      return
        Make_Subprogram_Body (Loc,
          Specification              => Make_Function_Specification (Loc,
            Defining_Unit_Name =>
              Make_Defining_Identifier (Loc, New_Internal_Name ('S')),
            Subtype_Mark       => New_Occurrence_Of (Var_Type, Loc)),
          Declarations               => No_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, New_List (
              Make_Tag_Check (Loc,
                Make_Return_Statement (Loc,
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Var_Type, Loc),
                    Attribute_Name => Name_Input,
                    Expressions    =>
                      New_List (Stream)))))));
   end Input_With_Tag_Check;

   --------------------------------
   -- Is_RACW_Controlling_Formal --
   --------------------------------

   function Is_RACW_Controlling_Formal
     (Parameter : Node_Id;
      Stub_Type : Entity_Id) return Boolean
   is
      Typ : Entity_Id;

   begin
      --  If the kind of the parameter is E_Void, then it is not a
      --  controlling formal (this can happen in the context of RAS).

      if Ekind (Defining_Identifier (Parameter)) = E_Void then
         return False;
      end if;

      --  If the parameter is not a controlling formal, then it cannot
      --  be possibly a RACW_Controlling_Formal.

      if not Is_Controlling_Formal (Defining_Identifier (Parameter)) then
         return False;
      end if;

      Typ := Parameter_Type (Parameter);
      return (Nkind (Typ) = N_Access_Definition
               and then Etype (Subtype_Mark (Typ)) = Stub_Type)
        or else Etype (Typ) = Stub_Type;
   end Is_RACW_Controlling_Formal;

   --------------------
   -- Make_Tag_Check --
   --------------------

   function Make_Tag_Check (Loc : Source_Ptr; N : Node_Id) return Node_Id is
      Occ : constant Entity_Id :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

   begin
      return Make_Block_Statement (Loc,
        Handled_Statement_Sequence =>
          Make_Handled_Sequence_Of_Statements (Loc,
            Statements         => New_List (N),

            Exception_Handlers => New_List (
              Make_Exception_Handler (Loc,
                Choice_Parameter => Occ,

                Exception_Choices =>
                  New_List (New_Occurrence_Of (RTE (RE_Tag_Error), Loc)),

                Statements =>
                  New_List (Make_Procedure_Call_Statement (Loc,
                    New_Occurrence_Of
                      (RTE (RE_Raise_Program_Error_Unknown_Tag), Loc),
                    New_List (New_Occurrence_Of (Occ, Loc))))))));
   end Make_Tag_Check;

   ----------------------------
   -- Need_Extra_Constrained --
   ----------------------------

   function Need_Extra_Constrained (Parameter : Node_Id) return Boolean is
      Etyp : constant Entity_Id := Etype (Parameter_Type (Parameter));
   begin
      return Out_Present (Parameter)
        and then Has_Discriminants (Etyp)
        and then not Is_Constrained (Etyp)
        and then not Is_Indefinite_Subtype (Etyp);
   end Need_Extra_Constrained;

   ------------------------------------
   -- Pack_Entity_Into_Stream_Access --
   ------------------------------------

   function Pack_Entity_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Entity_Id;
      Etyp   : Entity_Id := Empty) return Node_Id
   is
      Typ : Entity_Id;

   begin
      if Present (Etyp) then
         Typ := Etyp;
      else
         Typ := Etype (Object);
      end if;

      return
        Pack_Node_Into_Stream_Access (Loc,
          Stream => Stream,
          Object => New_Occurrence_Of (Object, Loc),
          Etyp   => Typ);
   end Pack_Entity_Into_Stream_Access;

   ---------------------------
   -- Pack_Node_Into_Stream --
   ---------------------------

   function Pack_Node_Into_Stream
     (Loc    : Source_Ptr;
      Stream : Entity_Id;
      Object : Node_Id;
      Etyp   : Entity_Id) return Node_Id
   is
      Write_Attribute : Name_Id := Name_Write;

   begin
      if not Is_Constrained (Etyp) then
         Write_Attribute := Name_Output;
      end if;

      return
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Etyp, Loc),
          Attribute_Name => Write_Attribute,
          Expressions    => New_List (
            Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Stream, Loc),
              Attribute_Name => Name_Access),
            Object));
   end Pack_Node_Into_Stream;

   ----------------------------------
   -- Pack_Node_Into_Stream_Access --
   ----------------------------------

   function Pack_Node_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Node_Id;
      Etyp   : Entity_Id) return Node_Id
   is
      Write_Attribute : Name_Id := Name_Write;

   begin
      if not Is_Constrained (Etyp) then
         Write_Attribute := Name_Output;
      end if;

      return
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Etyp, Loc),
          Attribute_Name => Write_Attribute,
          Expressions    => New_List (
            Stream,
            Object));
   end Pack_Node_Into_Stream_Access;

   ---------------------
   -- PolyORB_Support --
   ---------------------

   package body PolyORB_Support is

      --  Local subprograms

      procedure Add_RACW_Read_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id);
      --  Add Read attribute in Decls for the RACW type. The Read attribute
      --  is added right after the RACW_Type declaration while the body is
      --  inserted after Declarations.

      procedure Add_RACW_Write_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id);
      --  Same thing for the Write attribute

      procedure Add_RACW_From_Any
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id);
      --  Add the From_Any TSS for this RACW type

      procedure Add_RACW_To_Any
        (Designated_Type  : Entity_Id;
         RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id);
      --  Add the To_Any TSS for this RACW type

      procedure Add_RACW_TypeCode
        (Designated_Type : Entity_Id;
         RACW_Type       : Entity_Id;
         Declarations    : List_Id);
      --  Add the TypeCode TSS for this RACW type

      procedure Add_RAS_From_Any (RAS_Type : Entity_Id);
      --  Add the From_Any TSS for this RAS type

      procedure Add_RAS_To_Any   (RAS_Type : Entity_Id);
      --  Add the To_Any TSS for this RAS type

      procedure Add_RAS_TypeCode (RAS_Type : Entity_Id);
      --  Add the TypeCode TSS for this RAS type

      procedure Add_RAS_Access_TSS (N : Node_Id);
      --  Add a subprogram body for RAS Access TSS

      -------------------------------------
      -- Add_Obj_RPC_Receiver_Completion --
      -------------------------------------

      procedure Add_Obj_RPC_Receiver_Completion
        (Loc           : Source_Ptr;
         Decls         : List_Id;
         RPC_Receiver  : Entity_Id;
         Stub_Elements : Stub_Structure)
      is
         Desig : constant Entity_Id :=
           Etype (Designated_Type (Stub_Elements.RACW_Type));
      begin
         Append_To (Decls,
           Make_Procedure_Call_Statement (Loc,
              Name =>
                New_Occurrence_Of (
                  RTE (RE_Register_Obj_Receiving_Stub), Loc),

                Parameter_Associations => New_List (

               --  Name

                Make_String_Literal (Loc,
                  Full_Qualified_Name (Desig)),

               --  Handler

                Make_Attribute_Reference (Loc,
                  Prefix =>
                    New_Occurrence_Of (
                      Defining_Unit_Name (Parent (RPC_Receiver)), Loc),
                  Attribute_Name =>
                    Name_Access),

               --  Receiver

                Make_Attribute_Reference (Loc,
                  Prefix =>
                    New_Occurrence_Of (
                      Defining_Identifier (
                        Stub_Elements.RPC_Receiver_Decl), Loc),
                  Attribute_Name =>
                    Name_Access))));
      end Add_Obj_RPC_Receiver_Completion;

      -----------------------
      -- Add_RACW_Features --
      -----------------------

      procedure Add_RACW_Features
        (RACW_Type         : Entity_Id;
         Desig             : Entity_Id;
         Stub_Type         : Entity_Id;
         Stub_Type_Access  : Entity_Id;
         RPC_Receiver_Decl : Node_Id;
         Declarations      : List_Id)
      is
         pragma Warnings (Off);
         pragma Unreferenced (RPC_Receiver_Decl);
         pragma Warnings (On);

      begin
         Add_RACW_From_Any
           (RACW_Type           => RACW_Type,
            Stub_Type           => Stub_Type,
            Stub_Type_Access    => Stub_Type_Access,
            Declarations        => Declarations);

         Add_RACW_To_Any
           (Designated_Type     => Desig,
            RACW_Type           => RACW_Type,
            Stub_Type           => Stub_Type,
            Stub_Type_Access    => Stub_Type_Access,
            Declarations        => Declarations);

         --  In the PolyORB case, the RACW 'Read and 'Write attributes
         --  are implemented in terms of the From_Any and To_Any TSSs,
         --  so these TSSs must be expanded before 'Read and 'Write.

         Add_RACW_Write_Attribute
           (RACW_Type           => RACW_Type,
            Stub_Type           => Stub_Type,
            Stub_Type_Access    => Stub_Type_Access,
            Declarations        => Declarations);

         Add_RACW_Read_Attribute
           (RACW_Type           => RACW_Type,
            Stub_Type           => Stub_Type,
            Stub_Type_Access    => Stub_Type_Access,
            Declarations        => Declarations);

         Add_RACW_TypeCode
           (Designated_Type     => Desig,
            RACW_Type           => RACW_Type,
            Declarations        => Declarations);
      end Add_RACW_Features;

      -----------------------
      -- Add_RACW_From_Any --
      -----------------------

      procedure Add_RACW_From_Any
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id)
      is
         Loc    : constant Source_Ptr := Sloc (RACW_Type);
         Is_RAS : constant Boolean := not Comes_From_Source (RACW_Type);

         Fnam   : constant Entity_Id :=
                    Make_Defining_Identifier (Loc, New_Internal_Name ('F'));

         Func_Spec : Node_Id;
         Func_Decl : Node_Id;
         Func_Body : Node_Id;

         Decls            : List_Id;
         Statements       : List_Id;
         Stub_Statements  : List_Id;
         Local_Statements : List_Id;
         --  Various parts of the subprogram

         Any_Parameter  : constant Entity_Id :=
                            Make_Defining_Identifier (Loc, Name_A);
         Reference      : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('R'));
         Is_Local       : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('L'));
         Addr           : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('A'));
         Local_Stub     : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('L'));
         Stubbed_Result : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('S'));

         Stub_Condition : Node_Id;
         --  An expression that determines whether we create a stub for the
         --  newly-unpacked RACW. Normally we create a stub only for remote
         --  objects, but in the case of an RACW used to implement a RAS,
         --  we also create a stub for local subprograms if a pragma
         --  All_Calls_Remote applies.

         Asynchronous_Flag : constant Entity_Id :=
                               Asynchronous_Flags_Table.Get (RACW_Type);
         --  The flag object declared in Add_RACW_Asynchronous_Flag

      begin
         --  Object declarations

         Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Reference,
             Object_Definition =>
               New_Occurrence_Of (RTE (RE_Object_Ref), Loc),
             Expression =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Occurrence_Of (RTE (RE_FA_ObjRef), Loc),
                 Parameter_Associations => New_List (
                   New_Occurrence_Of (Any_Parameter, Loc)))),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Local_Stub,
             Aliased_Present     => True,
             Object_Definition   => New_Occurrence_Of (Stub_Type, Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Stubbed_Result,
             Object_Definition   =>
               New_Occurrence_Of (Stub_Type_Access, Loc),
             Expression          =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Local_Stub, Loc),
                 Attribute_Name =>
                   Name_Unchecked_Access)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Is_Local,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Boolean, Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Addr,
             Object_Definition =>
               New_Occurrence_Of (RTE (RE_Address), Loc)));

         --  Build_Get_Unique_RP_Call needs the type of Stubbed_Result

         Set_Etype (Stubbed_Result, Stub_Type_Access);

         --  If the ref Is_Nil, return a null pointer

         Statements := New_List (
           Make_Implicit_If_Statement (RACW_Type,
             Condition =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Occurrence_Of (RTE (RE_Is_Nil), Loc),
                 Parameter_Associations => New_List (
                   New_Occurrence_Of (Reference, Loc))),
             Then_Statements => New_List (
               Make_Return_Statement (Loc,
                 Expression =>
                   Make_Null (Loc)))));

         Append_To (Statements,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Get_Local_Address), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Reference, Loc),
               New_Occurrence_Of (Is_Local, Loc),
               New_Occurrence_Of (Addr, Loc))));

         --  If the object is located on another partition, then a stub object
         --  will be created with all the information needed to rebuild the
         --  real object at the other end. This stanza is always used in the
         --  case of RAS types, for which a stub is required even for local
         --  subprograms.

         Stub_Statements := New_List (
           Make_Assignment_Statement (Loc,
             Name       => Make_Selected_Component (Loc,
               Prefix        => Stubbed_Result,
               Selector_Name => Name_Target),
             Expression =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Occurrence_Of (RTE (RE_Entity_Of), Loc),
                 Parameter_Associations => New_List (
                   New_Occurrence_Of (Reference, Loc)))),

           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Inc_Usage), Loc),
             Parameter_Associations => New_List (
               Make_Selected_Component (Loc,
                 Prefix        => Stubbed_Result,
                 Selector_Name => Name_Target))),

           Make_Assignment_Statement (Loc,
             Name       => Make_Selected_Component (Loc,
               Prefix        => Stubbed_Result,
               Selector_Name => Name_Asynchronous),
             Expression =>
               New_Occurrence_Of (Asynchronous_Flag, Loc)));

         --  ??? Issue with asynchronous calls here: the Asynchronous
         --  flag is set on the stub type if, and only if, the RACW type
         --  has a pragma Asynchronous. This is incorrect for RACWs that
         --  implement RAS types, because in that case the /designated
         --  subprogram/ (not the type) might be asynchronous, and
         --  that causes the stub to need to be asynchronous too.
         --  A solution is to transport a RAS as a struct containing
         --  a RACW and an asynchronous flag, and to properly alter
         --  the Asynchronous component in the stub type in the RAS's
         --  _From_Any TSS.

         Append_List_To (Stub_Statements,
           Build_Get_Unique_RP_Call (Loc, Stubbed_Result, Stub_Type));

         --  Distinguish between the local and remote cases, and execute the
         --  appropriate piece of code.

         Stub_Condition := New_Occurrence_Of (Is_Local, Loc);

         if Is_RAS then
            Stub_Condition := Make_And_Then (Loc,
              Left_Opnd  =>
                Stub_Condition,
              Right_Opnd =>
                Make_Selected_Component (Loc,
                  Prefix =>
                    Unchecked_Convert_To (
                      RTE (RE_RAS_Proxy_Type_Access),
                      New_Occurrence_Of (Addr, Loc)),
                  Selector_Name =>
                    Make_Identifier (Loc,
                      Name_All_Calls_Remote)));
         end if;

         Local_Statements := New_List (
           Make_Return_Statement (Loc,
             Expression =>
               Unchecked_Convert_To (RACW_Type,
                 New_Occurrence_Of (Addr, Loc))));

         Append_To (Statements,
           Make_Implicit_If_Statement (RACW_Type,
             Condition =>
               Stub_Condition,
             Then_Statements => Local_Statements,
             Else_Statements => Stub_Statements));

         Append_To (Statements,
           Make_Return_Statement (Loc,
             Expression => Unchecked_Convert_To (RACW_Type,
               New_Occurrence_Of (Stubbed_Result, Loc))));

         Func_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name =>
               Fnam,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier =>
                   Any_Parameter,
                 Parameter_Type =>
                   New_Occurrence_Of (RTE (RE_Any), Loc))),
             Subtype_Mark => New_Occurrence_Of (RACW_Type, Loc));

         --  NOTE: The usage occurrences of RACW_Parameter must
         --  refer to the entity in the declaration spec, not those
         --  of the body spec.

         Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);

         Func_Body :=
           Make_Subprogram_Body (Loc,
             Specification              =>
               Copy_Specification (Loc, Func_Spec),
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Statements));

         Insert_After (Declaration_Node (RACW_Type), Func_Decl);
         Append_To (Declarations, Func_Body);

         Set_Renaming_TSS (RACW_Type, Fnam, TSS_From_Any);
      end Add_RACW_From_Any;

      -----------------------------
      -- Add_RACW_Read_Attribute --
      -----------------------------

      procedure Add_RACW_Read_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id)
      is
         pragma Warnings (Off);
         pragma Unreferenced (Stub_Type, Stub_Type_Access);
         pragma Warnings (On);
         Loc : constant Source_Ptr := Sloc (RACW_Type);

         Proc_Decl : Node_Id;
         Attr_Decl : Node_Id;

         Body_Node : Node_Id;

         Decls             : List_Id;
         Statements        : List_Id;
         --  Various parts of the procedure

         Procedure_Name    : constant Name_Id   :=
                               New_Internal_Name ('R');
         Source_Ref        : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('R'));
         Asynchronous_Flag : constant Entity_Id :=
                               Asynchronous_Flags_Table.Get (RACW_Type);
         pragma Assert (Present (Asynchronous_Flag));

         function Stream_Parameter return Node_Id;
         function Result return Node_Id;
         --  Functions to create occurrences of the formal parameter names

         ------------
         -- Result --
         ------------

         function Result return Node_Id is
         begin
            return Make_Identifier (Loc, Name_V);
         end Result;

         ----------------------
         -- Stream_Parameter --
         ----------------------

         function Stream_Parameter return Node_Id is
         begin
            return Make_Identifier (Loc, Name_S);
         end Stream_Parameter;

      --  Start of processing for Add_RACW_Read_Attribute

      begin
         --  Generate object declarations

         Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Source_Ref,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Object_Ref), Loc)));

         Statements := New_List (
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (RTE (RE_Object_Ref), Loc),
             Attribute_Name => Name_Read,
             Expressions    => New_List (
               Stream_Parameter,
               New_Occurrence_Of (Source_Ref, Loc))),
           Make_Assignment_Statement (Loc,
             Name =>
               Result,
             Expression =>
               PolyORB_Support.Helpers.Build_From_Any_Call (
                 RACW_Type,
                 Make_Function_Call (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_TA_ObjRef), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Source_Ref, Loc))),
                 Decls)));

         Build_Stream_Procedure
           (Loc, RACW_Type, Body_Node,
            Make_Defining_Identifier (Loc, Procedure_Name),
            Statements, Outp => True);
         Set_Declarations (Body_Node, Decls);

         Proc_Decl := Make_Subprogram_Declaration (Loc,
           Copy_Specification (Loc, Specification (Body_Node)));

         Attr_Decl :=
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Occurrence_Of (RACW_Type, Loc),
             Chars      => Name_Read,
             Expression =>
               New_Occurrence_Of (
                 Defining_Unit_Name (Specification (Proc_Decl)), Loc));

         Insert_After (Declaration_Node (RACW_Type), Proc_Decl);
         Insert_After (Proc_Decl, Attr_Decl);
         Append_To (Declarations, Body_Node);
      end Add_RACW_Read_Attribute;

      ---------------------
      -- Add_RACW_To_Any --
      ---------------------

      procedure Add_RACW_To_Any
        (Designated_Type  : Entity_Id;
         RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id)
      is
         Loc : constant Source_Ptr := Sloc (RACW_Type);

         Is_RAS : constant Boolean := not Comes_From_Source (RACW_Type);

         Fnam : Entity_Id;

         Stub_Elements : constant Stub_Structure :=
           Stubs_Table.Get (Designated_Type);
         pragma Assert (Stub_Elements /= Empty_Stub_Structure);

         Func_Spec : Node_Id;
         Func_Decl : Node_Id;
         Func_Body : Node_Id;

         Decls             : List_Id;
         Statements        : List_Id;
         Null_Statements   : List_Id;
         Local_Statements  : List_Id := No_List;
         Stub_Statements   : List_Id;
         If_Node           : Node_Id;
         --  Various parts of the subprogram

         RACW_Parameter : constant Entity_Id
           := Make_Defining_Identifier (Loc, Name_R);

         Reference         : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('R'));
         Any               : constant Entity_Id :=
                               Make_Defining_Identifier
                                 (Loc, New_Internal_Name ('A'));

      begin
         --  Object declarations

         Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Reference,
             Object_Definition =>
               New_Occurrence_Of (RTE (RE_Object_Ref), Loc)),
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Any,
             Object_Definition =>
               New_Occurrence_Of (RTE (RE_Any), Loc)));

         --  If the object is null, nothing to do (Reference is already
         --  a Nil ref.)

         Null_Statements := New_List (Make_Null_Statement (Loc));

         if Is_RAS then

            --  If the object is a RAS designating a local subprogram,
            --  we already have a target reference.

            Local_Statements := New_List (
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Set_Ref), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Reference, Loc),
                  Make_Selected_Component (Loc,
                    Prefix =>
                      Unchecked_Convert_To (RTE (RE_RAS_Proxy_Type_Access),
                        New_Occurrence_Of (RACW_Parameter, Loc)),
                    Selector_Name => Make_Identifier (Loc, Name_Target)))));

         else
            --  If the object is a local RACW object, use Get_Reference now
            --  to obtain a reference.

            Local_Statements := New_List (
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Get_Reference), Loc),
                Parameter_Associations => New_List (
                  Unchecked_Convert_To (
                    RTE (RE_Address),
                    New_Occurrence_Of (RACW_Parameter, Loc)),
                  Make_String_Literal (Loc,
                    Full_Qualified_Name (Designated_Type)),
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Occurrence_Of (
                        Defining_Identifier (
                          Stub_Elements.RPC_Receiver_Decl), Loc),
                    Attribute_Name =>
                      Name_Access),
                  New_Occurrence_Of (Reference, Loc))));
         end if;

         --  If the object is located on another partition, use the target
         --  from the stub.

         Stub_Statements := New_List (
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Set_Ref), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Reference, Loc),
               Make_Selected_Component (Loc,
                 Prefix        => Unchecked_Convert_To (Stub_Type_Access,
                   New_Occurrence_Of (RACW_Parameter, Loc)),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Target)))));

         --  Distinguish between the null, local and remote cases,
         --  and execute the appropriate piece of code.

         If_Node :=
           Make_Implicit_If_Statement (RACW_Type,
             Condition       =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Occurrence_Of (RACW_Parameter, Loc),
                 Right_Opnd => Make_Null (Loc)),
             Then_Statements => Null_Statements,
             Elsif_Parts     => New_List (
               Make_Elsif_Part (Loc,
                 Condition       =>
                   Make_Op_Ne (Loc,
                     Left_Opnd  =>
                        Make_Attribute_Reference (Loc,
                         Prefix         =>
                           New_Occurrence_Of (RACW_Parameter, Loc),
                         Attribute_Name => Name_Tag),
                     Right_Opnd =>
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Stub_Type, Loc),
                         Attribute_Name => Name_Tag)),
                 Then_Statements => Local_Statements)),
             Else_Statements => Stub_Statements);

         Statements := New_List (
           If_Node,
           Make_Assignment_Statement (Loc,
             Name =>
               New_Occurrence_Of (Any, Loc),
             Expression =>
               Make_Function_Call (Loc,
                 Name => New_Occurrence_Of (RTE (RE_TA_ObjRef), Loc),
                 Parameter_Associations => New_List (
                   New_Occurrence_Of (Reference, Loc)))),
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Set_TC), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Any, Loc),
               Make_Selected_Component (Loc,
                 Prefix =>
                     Defining_Identifier (
                       Stub_Elements.RPC_Receiver_Decl),
                 Selector_Name => Name_Obj_TypeCode))),
           Make_Return_Statement (Loc,
             Expression =>
               New_Occurrence_Of (Any, Loc)));

         Fnam := Make_Defining_Identifier (
           Loc, New_Internal_Name ('T'));

         Func_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name =>
               Fnam,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier =>
                   RACW_Parameter,
                 Parameter_Type =>
                   New_Occurrence_Of (RACW_Type, Loc))),
             Subtype_Mark => New_Occurrence_Of (RTE (RE_Any), Loc));

         --  NOTE: The usage occurrences of RACW_Parameter must
         --  refer to the entity in the declaration spec, not in
         --  the body spec.

         Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);

         Func_Body :=
           Make_Subprogram_Body (Loc,
             Specification              =>
               Copy_Specification (Loc, Func_Spec),
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Statements));

         Insert_After (Declaration_Node (RACW_Type), Func_Decl);
         Append_To (Declarations, Func_Body);

         Set_Renaming_TSS (RACW_Type, Fnam, TSS_To_Any);
      end Add_RACW_To_Any;

      -----------------------
      -- Add_RACW_TypeCode --
      -----------------------

      procedure Add_RACW_TypeCode
        (Designated_Type  : Entity_Id;
         RACW_Type        : Entity_Id;
         Declarations     : List_Id)
      is
         Loc : constant Source_Ptr := Sloc (RACW_Type);

         Fnam : Entity_Id;

         Stub_Elements : constant Stub_Structure :=
                           Stubs_Table.Get (Designated_Type);
         pragma Assert (Stub_Elements /= Empty_Stub_Structure);

         Func_Spec : Node_Id;
         Func_Decl : Node_Id;
         Func_Body : Node_Id;

         RACW_Parameter : constant Entity_Id :=
                            Make_Defining_Identifier (Loc, Name_R);

      begin
         Fnam :=
           Make_Defining_Identifier (Loc,
             Chars => New_Internal_Name ('T'));

         --  The spec for this subprogram has a dummy 'access RACW'
         --  argument, which serves only for overloading purposes.

         Func_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name =>
               Fnam,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier =>
                   RACW_Parameter,
                 Parameter_Type =>
                   Make_Access_Definition (Loc,
                     Subtype_Mark =>
                       New_Occurrence_Of (RACW_Type, Loc)))),
             Subtype_Mark => New_Occurrence_Of (RTE (RE_TypeCode), Loc));

         --  NOTE: The usage occurrences of RACW_Parameter must
         --  refer to the entity in the declaration spec, not those
         --  of the body spec.

         Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);

         Func_Body :=
           Make_Subprogram_Body (Loc,
             Specification              =>
               Copy_Specification (Loc, Func_Spec),
             Declarations               => Empty_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Return_Statement (Loc,
                     Expression =>
                       Make_Selected_Component (Loc,
                         Prefix =>
                             Defining_Identifier (
                               Stub_Elements.RPC_Receiver_Decl),
                         Selector_Name => Name_Obj_TypeCode)))));

         Insert_After (Declaration_Node (RACW_Type), Func_Decl);
         Append_To (Declarations, Func_Body);

         Set_Renaming_TSS (RACW_Type, Fnam, TSS_TypeCode);
      end Add_RACW_TypeCode;

      ------------------------------
      -- Add_RACW_Write_Attribute --
      ------------------------------

      procedure Add_RACW_Write_Attribute
        (RACW_Type        : Entity_Id;
         Stub_Type        : Entity_Id;
         Stub_Type_Access : Entity_Id;
         Declarations     : List_Id)
      is
         Loc : constant Source_Ptr := Sloc (RACW_Type);
         pragma Warnings (Off);
         pragma Unreferenced (
                  Stub_Type,
                  Stub_Type_Access);

         Is_RAS : constant Boolean := not Comes_From_Source (RACW_Type);
         pragma Unreferenced (Is_RAS);
         pragma Warnings (On);

         Body_Node : Node_Id;
         Proc_Decl : Node_Id;
         Attr_Decl : Node_Id;

         Statements     : List_Id;
         Procedure_Name : constant Name_Id := New_Internal_Name ('R');

         function Stream_Parameter return Node_Id;
         function Object return Node_Id;
         --  Functions to create occurrences of the formal parameter names

         ------------
         -- Object --
         ------------

         function Object return Node_Id is
            Object_Ref : constant Node_Id :=
                           Make_Identifier (Loc, Name_V);

         begin
            --  Etype must be set for Build_To_Any_Call

            Set_Etype (Object_Ref, RACW_Type);

            return Object_Ref;
         end Object;

         ----------------------
         -- Stream_Parameter --
         ----------------------

         function Stream_Parameter return Node_Id is
         begin
            return Make_Identifier (Loc, Name_S);
         end Stream_Parameter;

      --  Start of processing for Add_RACW_Write_Attribute

      begin
         Statements := New_List (
           Pack_Node_Into_Stream_Access (Loc,
             Stream => Stream_Parameter,
             Object =>
               Make_Function_Call (Loc,
                 Name =>
                   New_Occurrence_Of (RTE (RE_FA_ObjRef), Loc),
                 Parameter_Associations => New_List (
                   PolyORB_Support.Helpers.Build_To_Any_Call
                     (Object, Declarations))),
             Etyp => RTE (RE_Object_Ref)));

         Build_Stream_Procedure
           (Loc, RACW_Type, Body_Node,
            Make_Defining_Identifier (Loc, Procedure_Name),
            Statements, Outp => False);

         Proc_Decl :=
           Make_Subprogram_Declaration (Loc,
             Copy_Specification (Loc, Specification (Body_Node)));

         Attr_Decl :=
           Make_Attribute_Definition_Clause (Loc,
             Name       => New_Occurrence_Of (RACW_Type, Loc),
             Chars      => Name_Write,
             Expression =>
               New_Occurrence_Of (
                 Defining_Unit_Name (Specification (Proc_Decl)), Loc));

         Insert_After (Declaration_Node (RACW_Type), Proc_Decl);
         Insert_After (Proc_Decl, Attr_Decl);
         Append_To (Declarations, Body_Node);
      end Add_RACW_Write_Attribute;

      -----------------------
      -- Add_RAST_Features --
      -----------------------

      procedure Add_RAST_Features
        (Vis_Decl : Node_Id;
         RAS_Type : Entity_Id)
      is
      begin
         Add_RAS_Access_TSS (Vis_Decl);

         Add_RAS_From_Any (RAS_Type);
         Add_RAS_TypeCode (RAS_Type);

         --  To_Any uses TypeCode, and therefore needs to be generated last

         Add_RAS_To_Any   (RAS_Type);
      end Add_RAST_Features;

      ------------------------
      -- Add_RAS_Access_TSS --
      ------------------------

      procedure Add_RAS_Access_TSS (N : Node_Id) is
         Loc : constant Source_Ptr := Sloc (N);

         Ras_Type : constant Entity_Id := Defining_Identifier (N);
         Fat_Type : constant Entity_Id := Equivalent_Type (Ras_Type);
         --  Ras_Type is the access to subprogram type; Fat_Type is the
         --  corresponding record type.

         RACW_Type : constant Entity_Id :=
           Underlying_RACW_Type (Ras_Type);
         Desig     : constant Entity_Id :=
           Etype (Designated_Type (RACW_Type));

         Stub_Elements : constant Stub_Structure :=
           Stubs_Table.Get (Desig);
         pragma Assert (Stub_Elements /= Empty_Stub_Structure);

         Proc : constant Entity_Id :=
                  Make_Defining_Identifier (Loc,
                    Chars => Make_TSS_Name (Ras_Type, TSS_RAS_Access));

         Proc_Spec : Node_Id;

         --  Formal parameters

         Package_Name : constant Entity_Id :=
                          Make_Defining_Identifier (Loc,
                            Chars => Name_P);

         --  Target package

         Subp_Id : constant Entity_Id :=
                     Make_Defining_Identifier (Loc,
                       Chars => Name_S);

         --  Target subprogram

         Asynch_P : constant Entity_Id :=
                      Make_Defining_Identifier (Loc,
                        Chars => Name_Asynchronous);
         --  Is the procedure to which the 'Access applies asynchronous?

         All_Calls_Remote : constant Entity_Id :=
                              Make_Defining_Identifier (Loc,
                                Chars => Name_All_Calls_Remote);
         --  True if an All_Calls_Remote pragma applies to the RCI unit
         --  that contains the subprogram.

         --  Common local variables

         Proc_Decls      : List_Id;
         Proc_Statements : List_Id;

         Subp_Ref : constant Entity_Id :=
                      Make_Defining_Identifier (Loc, Name_R);
         --  Reference that designates the target subprogram (returned
         --  by Get_RAS_Info).

         Is_Local : constant Entity_Id :=
           Make_Defining_Identifier (Loc, Name_L);
         Local_Addr : constant Entity_Id :=
           Make_Defining_Identifier (Loc, Name_A);
         --  For the call to Get_Local_Address

         --  Additional local variables for the remote case

         Local_Stub : constant Entity_Id :=
                        Make_Defining_Identifier (Loc,
                          Chars => New_Internal_Name ('L'));

         Stub_Ptr : constant Entity_Id :=
                      Make_Defining_Identifier (Loc,
                        Chars => New_Internal_Name ('S'));

         function Set_Field
           (Field_Name : Name_Id;
            Value      : Node_Id) return Node_Id;
         --  Construct an assignment that sets the named component in the
         --  returned record

         ---------------
         -- Set_Field --
         ---------------

         function Set_Field
           (Field_Name : Name_Id;
            Value      : Node_Id) return Node_Id
         is
         begin
            return
              Make_Assignment_Statement (Loc,
                Name       =>
                  Make_Selected_Component (Loc,
                    Prefix        => Stub_Ptr,
                    Selector_Name => Field_Name),
                Expression => Value);
         end Set_Field;

      --  Start of processing for Add_RAS_Access_TSS

      begin
         Proc_Decls := New_List (

         --  Common declarations

           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Ref,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Object_Ref), Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Is_Local,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Boolean, Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Local_Addr,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Address), Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Local_Stub,
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (Stub_Elements.Stub_Type, Loc)),

           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Stub_Ptr,
             Object_Definition   =>
               New_Occurrence_Of (Stub_Elements.Stub_Type_Access, Loc),
             Expression          =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Local_Stub, Loc),
                 Attribute_Name => Name_Unchecked_Access)));

         Set_Etype (Stub_Ptr, Stub_Elements.Stub_Type_Access);
         --  Build_Get_Unique_RP_Call needs this information

         --  Get_RAS_Info (Pkg, Subp, R);
         --  Obtain a reference to the target subprogram

         Proc_Statements := New_List (
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Get_RAS_Info), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Package_Name, Loc),
               New_Occurrence_Of (Subp_Id, Loc),
               New_Occurrence_Of (Subp_Ref, Loc))),

         --  Get_Local_Address (R, L, A);
         --  Determine whether the subprogram is local (L), and if so
         --  obtain the local address of its proxy (A).

           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Get_Local_Address), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Subp_Ref, Loc),
               New_Occurrence_Of (Is_Local, Loc),
               New_Occurrence_Of (Local_Addr, Loc))));

         --  Note: Here we assume that the Fat_Type is a record containing just
         --  an access to a proxy or stub object.

         Append_To (Proc_Statements,

         --  if L then

           Make_Implicit_If_Statement (N,
             Condition =>
               New_Occurrence_Of (Is_Local, Loc),

             Then_Statements => New_List (

         --     if A.Target = null then

               Make_Implicit_If_Statement (N,
                 Condition =>
                   Make_Op_Eq (Loc,
                     Make_Selected_Component (Loc,
                       Prefix =>
                         Unchecked_Convert_To (
                           RTE (RE_RAS_Proxy_Type_Access),
                           New_Occurrence_Of (Local_Addr, Loc)),
                         Selector_Name =>
                           Make_Identifier (Loc, Name_Target)),
                     Make_Null (Loc)),

                 Then_Statements => New_List (

         --        A.Target := Entity_Of (Ref);

                   Make_Assignment_Statement (Loc,
                     Name =>
                       Make_Selected_Component (Loc,
                         Prefix =>
                           Unchecked_Convert_To (
                             RTE (RE_RAS_Proxy_Type_Access),
                             New_Occurrence_Of (Local_Addr, Loc)),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_Target)),
                     Expression =>
                       Make_Function_Call (Loc,
                         Name =>
                           New_Occurrence_Of (RTE (RE_Entity_Of), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (Subp_Ref, Loc)))),

         --        Inc_Usage (A.Target);

                   Make_Procedure_Call_Statement (Loc,
                     Name =>
                       New_Occurrence_Of (RTE (RE_Inc_Usage), Loc),
                     Parameter_Associations => New_List (
                       Make_Selected_Component (Loc,
                         Prefix        =>
                           Unchecked_Convert_To (
                             RTE (RE_RAS_Proxy_Type_Access),
                             New_Occurrence_Of (Local_Addr, Loc)),
                         Selector_Name => Make_Identifier (Loc,
                           Name_Target)))))),

         --     end if;
         --     if not All_Calls_Remote then
         --        return Fat_Type!(A);
         --     end if;

                 Make_Implicit_If_Statement (N,
                   Condition =>
                     Make_Op_Not (Loc,
                       New_Occurrence_Of (All_Calls_Remote, Loc)),

                   Then_Statements => New_List (
                     Make_Return_Statement (Loc,
                       Unchecked_Convert_To (Fat_Type,
                         New_Occurrence_Of (Local_Addr, Loc))))))));

         Append_List_To (Proc_Statements, New_List (

         --  Stub.Target := Entity_Of (Ref);

           Set_Field (Name_Target,
             Make_Function_Call (Loc,
               Name =>
                 New_Occurrence_Of (RTE (RE_Entity_Of), Loc),
               Parameter_Associations => New_List (
                 New_Occurrence_Of (Subp_Ref, Loc)))),

         --  Inc_Usage (Stub.Target);

           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Inc_Usage), Loc),
             Parameter_Associations => New_List (
               Make_Selected_Component (Loc,
                 Prefix        => Stub_Ptr,
                 Selector_Name => Name_Target))),

         --  E.4.1(9) A remote call is asynchronous if it is a call to
         --  a procedure, or a call through a value of an access-to-procedure
         --  type, to which a pragma Asynchronous applies.

         --    Parameter Asynch_P is true when the procedure is asynchronous;
         --    Expression Asynch_T is true when the type is asynchronous.

           Set_Field (Name_Asynchronous,
             Make_Or_Else (Loc,
               New_Occurrence_Of (Asynch_P, Loc),
               New_Occurrence_Of (Boolean_Literals (
                 Is_Asynchronous (Ras_Type)), Loc)))));

         Append_List_To (Proc_Statements,
           Build_Get_Unique_RP_Call (Loc,
             Stub_Ptr, Stub_Elements.Stub_Type));

         Append_To (Proc_Statements,
           Make_Return_Statement (Loc,
             Expression =>
               Unchecked_Convert_To (Fat_Type,
                 New_Occurrence_Of (Stub_Ptr, Loc))));

         Proc_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Proc,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Package_Name,
                 Parameter_Type      =>
                   New_Occurrence_Of (Standard_String, Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Subp_Id,
                 Parameter_Type      =>
                   New_Occurrence_Of (Standard_String, Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Asynch_P,
                 Parameter_Type      =>
                   New_Occurrence_Of (Standard_Boolean, Loc)),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => All_Calls_Remote,
                 Parameter_Type      =>
                   New_Occurrence_Of (Standard_Boolean, Loc))),

            Subtype_Mark =>
              New_Occurrence_Of (Fat_Type, Loc));

         --  Set the kind and return type of the function to prevent
         --  ambiguities between Ras_Type and Fat_Type in subsequent analysis.

         Set_Ekind (Proc, E_Function);
         Set_Etype (Proc, Fat_Type);

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification              => Proc_Spec,
             Declarations               => Proc_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Proc_Statements)));

         Set_TSS (Fat_Type, Proc);
      end Add_RAS_Access_TSS;

      ----------------------
      -- Add_RAS_From_Any --
      ----------------------

      procedure Add_RAS_From_Any (RAS_Type : Entity_Id) is
         Loc : constant Source_Ptr := Sloc (RAS_Type);

         Fnam : constant Entity_Id := Make_Defining_Identifier (Loc,
                  Make_TSS_Name (RAS_Type, TSS_From_Any));

         Func_Spec : Node_Id;

         Statements : List_Id;

         Any_Parameter : constant Entity_Id :=
                           Make_Defining_Identifier (Loc, Name_A);

      begin
         Statements := New_List (
           Make_Return_Statement (Loc,
             Expression =>
               Make_Aggregate (Loc,
                 Component_Associations => New_List (
                   Make_Component_Association (Loc,
                     Choices => New_List (
                       Make_Identifier (Loc, Name_Ras)),
                     Expression =>
                       PolyORB_Support.Helpers.Build_From_Any_Call (
                         Underlying_RACW_Type (RAS_Type),
                         New_Occurrence_Of (Any_Parameter, Loc),
                         No_List))))));

         Func_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name =>
               Fnam,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier =>
                   Any_Parameter,
                 Parameter_Type =>
                   New_Occurrence_Of (RTE (RE_Any), Loc))),
             Subtype_Mark => New_Occurrence_Of (RAS_Type, Loc));

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification              => Func_Spec,
             Declarations               => No_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Statements)));
         Set_TSS (RAS_Type, Fnam);
      end Add_RAS_From_Any;

      --------------------
      -- Add_RAS_To_Any --
      --------------------

      procedure Add_RAS_To_Any (RAS_Type : Entity_Id) is
         Loc : constant Source_Ptr := Sloc (RAS_Type);

         Fnam : constant Entity_Id := Make_Defining_Identifier (Loc,
                  Make_TSS_Name (RAS_Type, TSS_To_Any));

         Decls      : List_Id;
         Statements : List_Id;

         Func_Spec : Node_Id;

         Any            : constant Entity_Id :=
                            Make_Defining_Identifier (Loc,
                              Chars => New_Internal_Name ('A'));
         RAS_Parameter  : constant Entity_Id :=
                            Make_Defining_Identifier (Loc,
                              Chars => New_Internal_Name ('R'));
         RACW_Parameter : constant Node_Id :=
                            Make_Selected_Component (Loc,
                              Prefix        => RAS_Parameter,
                              Selector_Name => Name_Ras);

      begin
         --  Object declarations

         Set_Etype (RACW_Parameter, Underlying_RACW_Type (RAS_Type));
         Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Any,
             Object_Definition =>
               New_Occurrence_Of (RTE (RE_Any), Loc),
             Expression =>
               PolyORB_Support.Helpers.Build_To_Any_Call
                 (RACW_Parameter, No_List)));

         Statements := New_List (
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Set_TC), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Any, Loc),
               PolyORB_Support.Helpers.Build_TypeCode_Call (Loc,
                 RAS_Type, Decls))),
           Make_Return_Statement (Loc,
             Expression =>
               New_Occurrence_Of (Any, Loc)));

         Func_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name =>
               Fnam,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier =>
                   RAS_Parameter,
                 Parameter_Type =>
                   New_Occurrence_Of (RAS_Type, Loc))),
             Subtype_Mark => New_Occurrence_Of (RTE (RE_Any), Loc));

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification              => Func_Spec,
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Statements)));
         Set_TSS (RAS_Type, Fnam);
      end Add_RAS_To_Any;

      ----------------------
      -- Add_RAS_TypeCode --
      ----------------------

      procedure Add_RAS_TypeCode (RAS_Type : Entity_Id) is
         Loc : constant Source_Ptr := Sloc (RAS_Type);

         Fnam : constant Entity_Id := Make_Defining_Identifier (Loc,
                  Make_TSS_Name (RAS_Type, TSS_TypeCode));

         Func_Spec : Node_Id;

         Decls : constant List_Id := New_List;
         Name_String, Repo_Id_String : String_Id;

         RAS_Parameter : constant Entity_Id :=
                           Make_Defining_Identifier (Loc, Name_R);

      begin
         --  The spec for this subprogram has a dummy 'access RAS'
         --  argument, which serves only for overloading purposes.

         Func_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name =>
               Fnam,
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier =>
                   RAS_Parameter,
                 Parameter_Type =>
                   Make_Access_Definition (Loc,
                     Subtype_Mark => New_Occurrence_Of (RAS_Type, Loc)))),
             Subtype_Mark => New_Occurrence_Of (RTE (RE_TypeCode), Loc));

         PolyORB_Support.Helpers.Build_Name_And_Repository_Id
           (RAS_Type, Name_Str => Name_String, Repo_Id_Str => Repo_Id_String);

         Discard_Node (
           Make_Subprogram_Body (Loc,
             Specification              => Func_Spec,
             Declarations               => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Return_Statement (Loc,
                     Expression =>
                       Make_Function_Call (Loc,
                         Name =>
                           New_Occurrence_Of (RTE (RE_TC_Build), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (RTE (RE_TC_Object), Loc),
                           Make_Aggregate (Loc,
                             Expressions =>
                               New_List (
                                 Make_Function_Call (Loc,
                                   Name => New_Occurrence_Of (
                                     RTE (RE_TA_String), Loc),
                                   Parameter_Associations => New_List (
                                     Make_String_Literal (Loc, Name_String))),
                                 Make_Function_Call (Loc,
                                   Name => New_Occurrence_Of (
                                     RTE (RE_TA_String), Loc),
                                   Parameter_Associations => New_List (
                                     Make_String_Literal (Loc,
                                       Repo_Id_String))))))))))));
         Set_TSS (RAS_Type, Fnam);
      end Add_RAS_TypeCode;

      -----------------------------------------
      -- Add_Receiving_Stubs_To_Declarations --
      -----------------------------------------

      procedure Add_Receiving_Stubs_To_Declarations
        (Pkg_Spec : Node_Id;
         Decls    : List_Id)
      is
         Loc : constant Source_Ptr := Sloc (Pkg_Spec);

         Pkg_RPC_Receiver            : constant Entity_Id :=
                                         Make_Defining_Identifier (Loc,
                                           New_Internal_Name ('H'));
         Pkg_RPC_Receiver_Object     : Node_Id;

         Pkg_RPC_Receiver_Body       : Node_Id;
         Pkg_RPC_Receiver_Decls      : List_Id;
         Pkg_RPC_Receiver_Statements : List_Id;
         Pkg_RPC_Receiver_Cases      : constant List_Id := New_List;
         --  A Pkg_RPC_Receiver is built to decode the request

         Request                     : Node_Id;
         --  Request object received from neutral layer

         Subp_Id : Entity_Id;
         --  Subprogram identifier as received from the neutral
         --  distribution core.

         Subp_Index : Entity_Id;
         --  Internal index as determined by matching either the
         --  method name from the request structure, or the local
         --  subprogram address (in case of a RAS).

         Is_Local : constant Entity_Id :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('L'));
         Local_Address : constant Entity_Id :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
         --  Address of a local subprogram designated by a
         --  reference corresponding to a RAS.

         Dispatch_On_Address : constant List_Id := New_List;
         Dispatch_On_Name    : constant List_Id := New_List;

         Current_Declaration       : Node_Id;
         Current_Stubs             : Node_Id;
         Current_Subprogram_Number : Int := First_RCI_Subprogram_Id;

         Subp_Info_Array : constant Entity_Id :=
                             Make_Defining_Identifier (Loc,
                               Chars => New_Internal_Name ('I'));

         Subp_Info_List : constant List_Id := New_List;

         Register_Pkg_Actuals : constant List_Id := New_List;

         All_Calls_Remote_E  : Entity_Id;

         procedure Append_Stubs_To
           (RPC_Receiver_Cases : List_Id;
            Declaration        : Node_Id;
            Stubs              : Node_Id;
            Subp_Number        : Int;
            Subp_Dist_Name     : Entity_Id;
            Subp_Proxy_Addr    : Entity_Id);
         --  Add one case to the specified RPC receiver case list associating
         --  Subprogram_Number with the subprogram declared by Declaration, for
         --  which we have receiving stubs in Stubs. Subp_Number is an internal
         --  subprogram index. Subp_Dist_Name is the string used to call the
         --  subprogram by name, and Subp_Dist_Addr is the address of the proxy
         --  object, used in the context of calls through remote
         --  access-to-subprogram types.

         ---------------------
         -- Append_Stubs_To --
         ---------------------

         procedure Append_Stubs_To
           (RPC_Receiver_Cases : List_Id;
            Declaration        : Node_Id;
            Stubs              : Node_Id;
            Subp_Number        : Int;
            Subp_Dist_Name     : Entity_Id;
            Subp_Proxy_Addr    : Entity_Id)
         is
            Case_Stmts : List_Id;
         begin
            Case_Stmts := New_List (
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (
                    Defining_Entity (Stubs), Loc),
                Parameter_Associations =>
                  New_List (New_Occurrence_Of (Request, Loc))));
            if Nkind (Specification (Declaration))
              = N_Function_Specification
              or else not
                Is_Asynchronous (Defining_Entity (Specification (Declaration)))
            then
               Append_To (Case_Stmts, Make_Return_Statement (Loc));
            end if;

            Append_To (RPC_Receiver_Cases,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices =>
                   New_List (Make_Integer_Literal (Loc, Subp_Number)),
                Statements       =>
                  Case_Stmts));

            Append_To (Dispatch_On_Name,
              Make_Elsif_Part (Loc,
                Condition =>
                  Make_Function_Call (Loc,
                    Name =>
                      New_Occurrence_Of (RTE (RE_Caseless_String_Eq), Loc),
                    Parameter_Associations => New_List (
                      New_Occurrence_Of (Subp_Id, Loc),
                      New_Occurrence_Of (Subp_Dist_Name, Loc))),
                Then_Statements => New_List (
                  Make_Assignment_Statement (Loc,
                    New_Occurrence_Of (Subp_Index, Loc),
                    Make_Integer_Literal (Loc,
                       Subp_Number)))));

            Append_To (Dispatch_On_Address,
              Make_Elsif_Part (Loc,
                Condition =>
                  Make_Op_Eq (Loc,
                    Left_Opnd  =>
                      New_Occurrence_Of (Local_Address, Loc),
                    Right_Opnd =>
                      New_Occurrence_Of (Subp_Proxy_Addr, Loc)),
                Then_Statements => New_List (
                  Make_Assignment_Statement (Loc,
                    New_Occurrence_Of (Subp_Index, Loc),
                    Make_Integer_Literal (Loc,
                       Subp_Number)))));
         end Append_Stubs_To;

      --  Start of processing for Add_Receiving_Stubs_To_Declarations

      begin
         --  Building receiving stubs consist in several operations:

         --    - a package RPC receiver must be built. This subprogram
         --      will get a Subprogram_Id from the incoming stream
         --      and will dispatch the call to the right subprogram

         --    - a receiving stub for any subprogram visible in the package
         --      spec. This stub will read all the parameters from the stream,
         --      and put the result as well as the exception occurrence in the
         --      output stream

         --    - a dummy package with an empty spec and a body made of an
         --      elaboration part, whose job is to register the receiving
         --      part of this RCI package on the name server. This is done
         --      by calling System.Partition_Interface.Register_Receiving_Stub

         Build_RPC_Receiver_Body (
           RPC_Receiver => Pkg_RPC_Receiver,
           Request      => Request,
           Subp_Id      => Subp_Id,
           Subp_Index   => Subp_Index,
           Stmts        => Pkg_RPC_Receiver_Statements,
           Decl         => Pkg_RPC_Receiver_Body);
         Pkg_RPC_Receiver_Decls := Declarations (Pkg_RPC_Receiver_Body);

         --  Extract local address information from the target reference:
         --  if non-null, that means that this is a reference that denotes
         --  one particular operation, and hence that the operation name
         --  must not be taken into account for dispatching.

         Append_To (Pkg_RPC_Receiver_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Is_Local,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Boolean, Loc)));
         Append_To (Pkg_RPC_Receiver_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Local_Address,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Address), Loc)));
         Append_To (Pkg_RPC_Receiver_Statements,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Get_Local_Address), Loc),
             Parameter_Associations => New_List (
               Make_Selected_Component (Loc,
                 Prefix        => Request,
                 Selector_Name => Name_Target),
               New_Occurrence_Of (Is_Local, Loc),
               New_Occurrence_Of (Local_Address, Loc))));

         --  Determine whether the reference that was used to make
         --  the call was the base RCI reference (in which case
         --  Local_Address is 0, and the method identifier from the
         --  request must be used to determine which subprogram is
         --  called) or a reference identifying one particular subprogram
         --  (in which case Local_Address is the address of that
         --  subprogram, and the method name from the request is
         --  ignored).
         --  In each case, cascaded elsifs are used to determine the
         --  proper subprogram index. Using hash tables might be
         --  more efficient.

         Append_To (Pkg_RPC_Receiver_Statements,
           Make_Implicit_If_Statement (Pkg_Spec,
             Condition =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => New_Occurrence_Of (Local_Address, Loc),
                 Right_Opnd => New_Occurrence_Of (RTE (RE_Null_Address), Loc)),
             Then_Statements => New_List (
               Make_Implicit_If_Statement (Pkg_Spec,
                 Condition =>
                   New_Occurrence_Of (Standard_False, Loc),
                 Then_Statements => New_List (
                   Make_Null_Statement (Loc)),
                 Elsif_Parts =>
                   Dispatch_On_Address)),
             Else_Statements => New_List (
               Make_Implicit_If_Statement (Pkg_Spec,
                 Condition =>
                   New_Occurrence_Of (Standard_False, Loc),
                 Then_Statements => New_List (
                   Make_Null_Statement (Loc)),
                 Elsif_Parts =>
                   Dispatch_On_Name))));

         --  For each subprogram, the receiving stub will be built and a
         --  case statement will be made on the Subprogram_Id to dispatch
         --  to the right subprogram.

         All_Calls_Remote_E := Boolean_Literals (
           Has_All_Calls_Remote (Defining_Entity (Pkg_Spec)));

         Overload_Counter_Table.Reset;
         Reserve_NamingContext_Methods;

         Current_Declaration := First (Visible_Declarations (Pkg_Spec));
         while Present (Current_Declaration) loop
            if Nkind (Current_Declaration) = N_Subprogram_Declaration
              and then Comes_From_Source (Current_Declaration)
            then
               declare
                  Loc : constant Source_Ptr :=
                          Sloc (Current_Declaration);
                  --  While specifically processing Current_Declaration, use
                  --  its Sloc as the location of all generated nodes.

                  Subp_Def : constant Entity_Id :=
                               Defining_Unit_Name
                                 (Specification (Current_Declaration));

                  Subp_Val : String_Id;

                  Subp_Dist_Name : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      New_External_Name (
                        Related_Id   => Chars (Subp_Def),
                        Suffix       => 'D',
                        Suffix_Index => -1));

                  Proxy_Object_Addr : Entity_Id;

               begin
                  pragma Assert (Current_Subprogram_Number =
                    Get_Subprogram_Id (Subp_Def));

                  --  Build receiving stub

                  Current_Stubs :=
                    Build_Subprogram_Receiving_Stubs
                      (Vis_Decl     => Current_Declaration,
                       Asynchronous =>
                         Nkind (Specification (Current_Declaration)) =
                             N_Procedure_Specification
                           and then Is_Asynchronous (Subp_Def));

                  Append_To (Decls, Current_Stubs);
                  Analyze (Current_Stubs);

                  --  Build RAS proxy

                  Add_RAS_Proxy_And_Analyze (Decls,
                    Vis_Decl           =>
                      Current_Declaration,
                    All_Calls_Remote_E =>
                      All_Calls_Remote_E,
                    Proxy_Object_Addr  =>
                      Proxy_Object_Addr);

                  --  Compute distribution identifier

                  Assign_Subprogram_Identifier (
                    Subp_Def,
                    Current_Subprogram_Number,
                    Subp_Val);

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Subp_Dist_Name,
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (
                        Standard_String, Loc),
                      Expression          =>
                        Make_String_Literal (Loc, Subp_Val)));
                  Analyze (Last (Decls));

                  --  Add subprogram descriptor (RCI_Subp_Info) to the
                  --  subprograms table for this receiver. The aggregate
                  --  below must be kept consistent with the declaration
                  --  of type RCI_Subp_Info in System.Partition_Interface.

                  Append_To (Subp_Info_List,
                    Make_Component_Association (Loc,
                      Choices => New_List (
                        Make_Integer_Literal (Loc,
                          Current_Subprogram_Number)),
                      Expression =>
                        Make_Aggregate (Loc,
                          Expressions => New_List (
                            Make_Attribute_Reference (Loc,
                              Prefix =>
                                New_Occurrence_Of (
                                  Subp_Dist_Name, Loc),
                              Attribute_Name => Name_Address),
                            Make_Attribute_Reference (Loc,
                              Prefix =>
                                New_Occurrence_Of (
                                  Subp_Dist_Name, Loc),
                              Attribute_Name => Name_Length),
                            New_Occurrence_Of (Proxy_Object_Addr, Loc)))));

                  Append_Stubs_To (Pkg_RPC_Receiver_Cases,
                    Declaration     => Current_Declaration,
                    Stubs           => Current_Stubs,
                    Subp_Number     => Current_Subprogram_Number,
                    Subp_Dist_Name  => Subp_Dist_Name,
                    Subp_Proxy_Addr => Proxy_Object_Addr);
               end;

               Current_Subprogram_Number := Current_Subprogram_Number + 1;
            end if;

            Next (Current_Declaration);
         end loop;

         --  If we receive an invalid Subprogram_Id, it is best to do nothing
         --  rather than raising an exception since we do not want someone
         --  to crash a remote partition by sending invalid subprogram ids.
         --  This is consistent with the other parts of the case statement
         --  since even in presence of incorrect parameters in the stream,
         --  every exception will be caught and (if the subprogram is not an
         --  APC) put into the result stream and sent away.

         Append_To (Pkg_RPC_Receiver_Cases,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices =>
               New_List (Make_Others_Choice (Loc)),
             Statements       =>
               New_List (Make_Null_Statement (Loc))));

         Append_To (Pkg_RPC_Receiver_Statements,
           Make_Case_Statement (Loc,
             Expression   =>
               New_Occurrence_Of (Subp_Index, Loc),
             Alternatives => Pkg_RPC_Receiver_Cases));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Info_Array,
             Constant_Present    => True,
             Aliased_Present     => True,
             Object_Definition   =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_RCI_Subp_Info_Array), Loc),
                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     New_List (
                       Make_Range (Loc,
                         Low_Bound  => Make_Integer_Literal (Loc,
                           First_RCI_Subprogram_Id),
                         High_Bound =>
                           Make_Integer_Literal (Loc,
                             First_RCI_Subprogram_Id
                             + List_Length (Subp_Info_List) - 1))))),
             Expression          =>
               Make_Aggregate (Loc,
                 Component_Associations => Subp_Info_List)));
         Analyze (Last (Decls));

         Append_To (Decls, Pkg_RPC_Receiver_Body);
         Analyze (Last (Decls));

         Pkg_RPC_Receiver_Object :=
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, New_Internal_Name ('R')),
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Servant), Loc));
         Append_To (Decls, Pkg_RPC_Receiver_Object);
         Analyze (Last (Decls));

         Get_Library_Unit_Name_String (Pkg_Spec);
         Append_To (Register_Pkg_Actuals,
            --  Name
           Make_String_Literal (Loc,
             Strval => String_From_Name_Buffer));

         Append_To (Register_Pkg_Actuals,
            --  Version
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of
                 (Defining_Entity (Pkg_Spec), Loc),
             Attribute_Name =>
               Name_Version));

         Append_To (Register_Pkg_Actuals,
            --  Handler
           Make_Attribute_Reference (Loc,
             Prefix          =>
               New_Occurrence_Of (Pkg_RPC_Receiver, Loc),
             Attribute_Name  => Name_Access));

         Append_To (Register_Pkg_Actuals,
            --  Receiver
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (
                 Defining_Identifier (
                   Pkg_RPC_Receiver_Object), Loc),
             Attribute_Name =>
               Name_Access));

         Append_To (Register_Pkg_Actuals,
            --  Subp_Info
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Subp_Info_Array, Loc),
             Attribute_Name =>
               Name_Address));

         Append_To (Register_Pkg_Actuals,
            --  Subp_Info_Len
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Subp_Info_Array, Loc),
             Attribute_Name =>
               Name_Length));

         Append_To (Register_Pkg_Actuals,
            --  Is_All_Calls_Remote
           New_Occurrence_Of (All_Calls_Remote_E, Loc));

         Append_To (Decls,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Register_Pkg_Receiving_Stub), Loc),
             Parameter_Associations => Register_Pkg_Actuals));
         Analyze (Last (Decls));

      end Add_Receiving_Stubs_To_Declarations;

      ---------------------------------
      -- Build_General_Calling_Stubs --
      ---------------------------------

      procedure Build_General_Calling_Stubs
        (Decls                     : List_Id;
         Statements                : List_Id;
         Target_Object             : Node_Id;
         Subprogram_Id             : Node_Id;
         Asynchronous              : Node_Id   := Empty;
         Is_Known_Asynchronous     : Boolean   := False;
         Is_Known_Non_Asynchronous : Boolean   := False;
         Is_Function               : Boolean;
         Spec                      : Node_Id;
         Stub_Type                 : Entity_Id := Empty;
         RACW_Type                 : Entity_Id := Empty;
         Nod                       : Node_Id)
      is
         Loc : constant Source_Ptr := Sloc (Nod);

         Arguments : Node_Id;
         --  Name of the named values list used to transmit parameters
         --  to the remote package

         Request : Node_Id;
         --  The request object constructed by these stubs

         Result : Node_Id;
         --  Name of the result named value (in non-APC cases) which get the
         --  result of the remote subprogram.

         Result_TC : Node_Id;
         --  Typecode expression for the result of the request (void
         --  typecode for procedures).

         Exception_Return_Parameter : Node_Id;
         --  Name of the parameter which will hold the exception sent by the
         --  remote subprogram.

         Current_Parameter : Node_Id;
         --  Current parameter being handled

         Ordered_Parameters_List : constant List_Id :=
                                     Build_Ordered_Parameters_List (Spec);

         Asynchronous_P : Node_Id;
         --  A Boolean expression indicating whether this call is asynchronous

         Asynchronous_Statements     : List_Id := No_List;
         Non_Asynchronous_Statements : List_Id := No_List;
         --  Statements specifics to the Asynchronous/Non-Asynchronous cases

         Extra_Formal_Statements : constant List_Id := New_List;
         --  List of statements for extra formal parameters. It will appear
         --  after the regular statements for writing out parameters.

         After_Statements : constant List_Id := New_List;
         --  Statements to be executed after call returns (to assign
         --  in out or out parameter values).

         Etyp : Entity_Id;
         --  The type of the formal parameter being processed

         Is_Controlling_Formal         : Boolean;
         Is_First_Controlling_Formal   : Boolean;
         First_Controlling_Formal_Seen : Boolean := False;
         --  Controlling formal parameters of distributed object
         --  primitives require special handling, and the first
         --  such parameter needs even more.

      begin
         --  ??? document general form of stub subprograms for the PolyORB case
         Request :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Request,
             Aliased_Present     => False,
             Object_Definition   =>
                 New_Occurrence_Of (RTE (RE_Request_Access), Loc)));

         Result :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         if Is_Function then
            Result_TC := PolyORB_Support.Helpers.Build_TypeCode_Call (Loc,
              Etype (Subtype_Mark (Spec)), Decls);
         else
            Result_TC := New_Occurrence_Of (RTE (RE_TC_Void), Loc);
         end if;

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Result,
             Aliased_Present     => False,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_NamedValue), Loc),
             Expression =>
               Make_Aggregate (Loc,
                 Component_Associations => New_List (
                   Make_Component_Association (Loc,
                     Choices => New_List (
                       Make_Identifier (Loc, Name_Name)),
                     Expression =>
                       New_Occurrence_Of (RTE (RE_Result_Name), Loc)),
                   Make_Component_Association (Loc,
                     Choices => New_List (
                       Make_Identifier (Loc, Name_Argument)),
                     Expression =>
                       Make_Function_Call (Loc,
                         Name =>
                           New_Occurrence_Of (RTE (RE_Create_Any), Loc),
                         Parameter_Associations => New_List (
                           Result_TC))),
                   Make_Component_Association (Loc,
                     Choices => New_List (
                       Make_Identifier (Loc, Name_Arg_Modes)),
                     Expression =>
                       Make_Integer_Literal (Loc, 0))))));

         if not Is_Known_Asynchronous then
            Exception_Return_Parameter :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Exception_Return_Parameter,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc)));

         else
            Exception_Return_Parameter := Empty;
         end if;

         --  Initialize and fill in arguments list

         Arguments :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
         Declare_Create_NVList (Loc, Arguments, Decls, Statements);

         Current_Parameter := First (Ordered_Parameters_List);
         while Present (Current_Parameter) loop

            if Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type) then
               Is_Controlling_Formal := True;
               Is_First_Controlling_Formal :=
                 not First_Controlling_Formal_Seen;
               First_Controlling_Formal_Seen := True;
            else
               Is_Controlling_Formal := False;
               Is_First_Controlling_Formal := False;
            end if;

            if Is_Controlling_Formal then

               --  In the case of a controlling formal argument, we send
               --  its reference.

               Etyp := RACW_Type;

            else
               Etyp := Etype (Parameter_Type (Current_Parameter));
            end if;

            --  The first controlling formal parameter is treated
            --  specially: it is used to set the target object of
            --  the call.

            if not Is_First_Controlling_Formal then

               declare
                  Constrained : constant Boolean :=
                                  Is_Constrained (Etyp)
                                    or else Is_Elementary_Type (Etyp);

                  Any : constant Entity_Id :=
                          Make_Defining_Identifier (Loc,
                            New_Internal_Name ('A'));

                  Actual_Parameter : Node_Id :=
                                       New_Occurrence_Of (
                                         Defining_Identifier (
                                           Current_Parameter), Loc);

                  Expr : Node_Id;

               begin
                  if Is_Controlling_Formal then

                     --  For a controlling formal parameter (other
                     --  than the first one), use the corresponding
                     --  RACW. If the parameter is not an anonymous
                     --  access parameter, that involves taking
                     --  its 'Unrestricted_Access.

                     if Nkind (Parameter_Type (Current_Parameter))
                       = N_Access_Definition
                     then
                        Actual_Parameter := OK_Convert_To
                          (Etyp, Actual_Parameter);
                     else
                        Actual_Parameter := OK_Convert_To (Etyp,
                          Make_Attribute_Reference (Loc,
                            Prefix =>
                              Actual_Parameter,
                            Attribute_Name =>
                              Name_Unrestricted_Access));
                     end if;

                  end if;

                  if In_Present (Current_Parameter)
                    or else not Out_Present (Current_Parameter)
                    or else not Constrained
                    or else Is_Controlling_Formal
                  then
                     --  The parameter has an input value, is constrained
                     --  at runtime by an input value, or is a controlling
                     --  formal parameter (always passed as a reference)
                     --  other than the first one.

                     Expr := PolyORB_Support.Helpers.Build_To_Any_Call (
                               Actual_Parameter, Decls);
                  else
                     Expr := Make_Function_Call (Loc,
                       Name =>
                         New_Occurrence_Of (RTE (RE_Create_Any), Loc),
                       Parameter_Associations => New_List (
                         PolyORB_Support.Helpers.Build_TypeCode_Call (Loc,
                           Etyp, Decls)));
                  end if;

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Any,
                      Aliased_Present     => False,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Any), Loc),
                      Expression          =>
                        Expr));

                  Append_To (Statements,
                    Add_Parameter_To_NVList (Loc,
                      Parameter   => Current_Parameter,
                      NVList      => Arguments,
                      Constrained => Constrained,
                      Any         => Any));

                  if Out_Present (Current_Parameter)
                    and then not Is_Controlling_Formal
                  then
                     Append_To (After_Statements,
                       Make_Assignment_Statement (Loc,
                         Name =>
                           New_Occurrence_Of (
                             Defining_Identifier (Current_Parameter), Loc),
                           Expression =>
                             PolyORB_Support.Helpers.Build_From_Any_Call (
                               Etype (Parameter_Type (Current_Parameter)),
                               New_Occurrence_Of (Any, Loc),
                               Decls)));

                  end if;
               end;
            end if;

            --  If the current parameter has a dynamic constrained status,
            --  then this status is transmitted as well.
            --  This should be done for accessibility as well ???

            if Nkind (Parameter_Type (Current_Parameter))
              /= N_Access_Definition
              and then Need_Extra_Constrained (Current_Parameter)
            then
               --  In this block, we do not use the extra formal that has been
               --  created because it does not exist at the time of expansion
               --  when building calling stubs for remote access to subprogram
               --  types. We create an extra variable of this type and push it
               --  in the stream after the regular parameters.

               declare
                  Extra_Any_Parameter : constant Entity_Id :=
                                      Make_Defining_Identifier
                                        (Loc, New_Internal_Name ('P'));

               begin
                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Extra_Any_Parameter,
                      Aliased_Present     => False,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Any), Loc),
                      Expression          =>
                        PolyORB_Support.Helpers.Build_To_Any_Call (
                          Make_Attribute_Reference (Loc,
                            Prefix         =>
                              New_Occurrence_Of (
                                Defining_Identifier (Current_Parameter), Loc),
                            Attribute_Name => Name_Constrained),
                          Decls)));
                  Append_To (Extra_Formal_Statements,
                    Add_Parameter_To_NVList (Loc,
                      Parameter   => Extra_Any_Parameter,
                      NVList      => Arguments,
                      Constrained => True,
                      Any         => Extra_Any_Parameter));
               end;
            end if;

            Next (Current_Parameter);
         end loop;

         --  Append the formal statements list to the statements

         Append_List_To (Statements, Extra_Formal_Statements);

         Append_To (Statements,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Request_Create), Loc),
             Parameter_Associations => New_List (
               Target_Object,
               Subprogram_Id,
               New_Occurrence_Of (Arguments, Loc),
               New_Occurrence_Of (Result, Loc),
               New_Occurrence_Of (RTE (RE_Nil_Exc_List), Loc))));

         Append_To (Parameter_Associations (Last (Statements)),
               New_Occurrence_Of (Request, Loc));

         pragma Assert (
           not (Is_Known_Non_Asynchronous and Is_Known_Asynchronous));
         if Is_Known_Non_Asynchronous or Is_Known_Asynchronous then
            Asynchronous_P := New_Occurrence_Of (
              Boolean_Literals (Is_Known_Asynchronous), Loc);
         else
            pragma Assert (Present (Asynchronous));
            Asynchronous_P := New_Copy_Tree (Asynchronous);
            --  The expression node Asynchronous will be used to build
            --  an 'if' statement at the end of Build_General_Calling_Stubs:
            --  we need to make a copy here.
         end if;

         Append_To (Parameter_Associations (Last (Statements)),
           Make_Indexed_Component (Loc,
             Prefix =>
               New_Occurrence_Of (
                 RTE (RE_Asynchronous_P_To_Sync_Scope), Loc),
             Expressions => New_List (Asynchronous_P)));

         Append_To (Statements,
             Make_Procedure_Call_Statement (Loc,
               Name                   =>
                 New_Occurrence_Of (RTE (RE_Request_Invoke), Loc),
               Parameter_Associations => New_List (
                 New_Occurrence_Of (Request, Loc))));

         Non_Asynchronous_Statements := New_List (Make_Null_Statement (Loc));
         Asynchronous_Statements := New_List (Make_Null_Statement (Loc));

         if not Is_Known_Asynchronous then

            --  Reraise an exception occurrence from the completed request.
            --  If the exception occurrence is empty, this is a no-op.

            Append_To (Non_Asynchronous_Statements,
              Make_Procedure_Call_Statement (Loc,
                Name                   =>
                  New_Occurrence_Of (RTE (RE_Request_Raise_Occurrence), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Request, Loc))));

            if Is_Function then

               --  If this is a function call, then read the value and
               --  return it.

               Append_To (Non_Asynchronous_Statements,
                 Make_Tag_Check (Loc,
                   Make_Return_Statement (Loc,
                     PolyORB_Support.Helpers.Build_From_Any_Call (
                         Etype (Subtype_Mark (Spec)),
                         Make_Selected_Component (Loc,
                           Prefix        => Result,
                           Selector_Name => Name_Argument),
                         Decls))));
            end if;
         end if;

         Append_List_To (Non_Asynchronous_Statements,
           After_Statements);

         if Is_Known_Asynchronous then
            Append_List_To (Statements, Asynchronous_Statements);

         elsif Is_Known_Non_Asynchronous then
            Append_List_To (Statements, Non_Asynchronous_Statements);

         else
            pragma Assert (Present (Asynchronous));
            Append_To (Statements,
              Make_Implicit_If_Statement (Nod,
                Condition       => Asynchronous,
                Then_Statements => Asynchronous_Statements,
                Else_Statements => Non_Asynchronous_Statements));
         end if;
      end Build_General_Calling_Stubs;

      -----------------------
      -- Build_Stub_Target --
      -----------------------

      function Build_Stub_Target
        (Loc                   : Source_Ptr;
         Decls                 : List_Id;
         RCI_Locator           : Entity_Id;
         Controlling_Parameter : Entity_Id) return RPC_Target
      is
         Target_Info : RPC_Target (PCS_Kind => Name_PolyORB_DSA);
         Target_Reference : constant Entity_Id :=
                              Make_Defining_Identifier (Loc,
                                New_Internal_Name ('T'));
      begin
         if Present (Controlling_Parameter) then
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Target_Reference,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Object_Ref), Loc),
                Expression          =>
                  Make_Function_Call (Loc,
                    Name =>
                      New_Occurrence_Of (RTE (RE_Make_Ref), Loc),
                    Parameter_Associations => New_List (
                      Make_Selected_Component (Loc,
                        Prefix        => Controlling_Parameter,
                        Selector_Name => Name_Target)))));
            --  Controlling_Parameter has the same components
            --  as System.Partition_Interface.RACW_Stub_Type.

            Target_Info.Object := New_Occurrence_Of (Target_Reference, Loc);

         else
            Target_Info.Object :=
              Make_Selected_Component (Loc,
                Prefix        =>
                  Make_Identifier (Loc, Chars (RCI_Locator)),
                Selector_Name =>
                  Make_Identifier (Loc, Name_Get_RCI_Package_Ref));
         end if;
         return Target_Info;
      end Build_Stub_Target;

      ---------------------
      -- Build_Stub_Type --
      ---------------------

      procedure Build_Stub_Type
        (RACW_Type         : Entity_Id;
         Stub_Type         : Entity_Id;
         Stub_Type_Decl    : out Node_Id;
         RPC_Receiver_Decl : out Node_Id)
      is
         Loc : constant Source_Ptr := Sloc (Stub_Type);
         pragma Warnings (Off);
         pragma Unreferenced (RACW_Type);
         pragma Warnings (On);

      begin
         Stub_Type_Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Stub_Type,
             Type_Definition     =>
               Make_Record_Definition (Loc,
                 Tagged_Present  => True,
                 Limited_Present => True,
                 Component_List  =>
                   Make_Component_List (Loc,
                     Component_Items => New_List (

                       Make_Component_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Name_Target),
                         Component_Definition =>
                           Make_Component_Definition (Loc,
                             Aliased_Present     =>
                               False,
                             Subtype_Indication  =>
                               New_Occurrence_Of (RTE (RE_Entity_Ptr), Loc))),

                       Make_Component_Declaration (Loc,
                         Defining_Identifier =>
                           Make_Defining_Identifier (Loc, Name_Asynchronous),
                         Component_Definition =>
                           Make_Component_Definition (Loc,
                             Aliased_Present    => False,
                             Subtype_Indication =>
                               New_Occurrence_Of (
                                 Standard_Boolean, Loc)))))));

         RPC_Receiver_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc,
                                      New_Internal_Name ('R')),
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Servant), Loc));
      end Build_Stub_Type;

      -----------------------------
      -- Build_RPC_Receiver_Body --
      -----------------------------

      procedure Build_RPC_Receiver_Body
        (RPC_Receiver : Entity_Id;
         Request      : out Entity_Id;
         Subp_Id      : out Entity_Id;
         Subp_Index   : out Entity_Id;
         Stmts        : out List_Id;
         Decl         : out Node_Id)
      is
         Loc : constant Source_Ptr := Sloc (RPC_Receiver);

         RPC_Receiver_Spec  : Node_Id;
         RPC_Receiver_Decls : List_Id;

      begin
         Request := Make_Defining_Identifier (Loc, Name_R);

         RPC_Receiver_Spec :=
           Build_RPC_Receiver_Specification (
             RPC_Receiver      => RPC_Receiver,
             Request_Parameter => Request);

         Subp_Id    := Make_Defining_Identifier (Loc, Name_P);
         Subp_Index := Make_Defining_Identifier (Loc, Name_I);

         RPC_Receiver_Decls := New_List (
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Subp_Id,
             Subtype_Mark        => New_Occurrence_Of (Standard_String, Loc),
             Name                =>
               Make_Explicit_Dereference (Loc,
                 Prefix =>
                   Make_Selected_Component (Loc,
                     Prefix        => Request,
                     Selector_Name => Name_Operation))),

           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Index,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc),
             Expression          =>
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc),
                 Attribute_Name => Name_Last)));

         Stmts := New_List;

         Decl :=
           Make_Subprogram_Body (Loc,
             Specification              => RPC_Receiver_Spec,
             Declarations               => RPC_Receiver_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stmts));
      end Build_RPC_Receiver_Body;

      --------------------------------------
      -- Build_Subprogram_Receiving_Stubs --
      --------------------------------------

      function Build_Subprogram_Receiving_Stubs
        (Vis_Decl                 : Node_Id;
         Asynchronous             : Boolean;
         Dynamically_Asynchronous : Boolean   := False;
         Stub_Type                : Entity_Id := Empty;
         RACW_Type                : Entity_Id := Empty;
         Parent_Primitive         : Entity_Id := Empty) return Node_Id
      is
         Loc : constant Source_Ptr := Sloc (Vis_Decl);

         Request_Parameter : Node_Id;
         --  ???

         Outer_Decls : constant List_Id := New_List;
         --  At the outermost level, an NVList and Any's are
         --  declared for all parameters. The Dynamic_Async
         --  flag also needs to be declared there to be visible
         --  from the exception handling code.

         Outer_Statements : constant List_Id := New_List;
         --  Statements that occur prior to the declaration of the actual
         --  parameter variables.

         Decls : constant List_Id := New_List;
         --  All the parameters will get declared before calling the real
         --  subprograms. Also the out parameters will be declared.
         --  At this level, parameters may be unconstrained.

         Statements : constant List_Id := New_List;

         Extra_Formal_Statements : constant List_Id := New_List;
         --  Statements concerning extra formal parameters

         After_Statements : constant List_Id := New_List;
         --  Statements to be executed after the subprogram call

         Inner_Decls : List_Id := No_List;
         --  In case of a function, the inner declarations are needed since
         --  the result may be unconstrained.

         Excep_Handlers : List_Id := No_List;

         Parameter_List : constant List_Id := New_List;
         --  List of parameters to be passed to the subprogram

         First_Controlling_Formal_Seen : Boolean := False;

         Current_Parameter : Node_Id;

         Ordered_Parameters_List : constant List_Id :=
                                     Build_Ordered_Parameters_List
                                       (Specification (Vis_Decl));

         Arguments : Node_Id;
         --  Name of the named values list used to retrieve parameters

         Subp_Spec : Node_Id;
         --  Subprogram specification

         Called_Subprogram : Node_Id;
         --  The subprogram to call

      begin
         if Present (RACW_Type) then
            Called_Subprogram :=
              New_Occurrence_Of (Parent_Primitive, Loc);
         else
            Called_Subprogram :=
              New_Occurrence_Of (
                Defining_Unit_Name (Specification (Vis_Decl)), Loc);
         end if;

         Request_Parameter :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         Arguments :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
         Declare_Create_NVList (Loc, Arguments, Outer_Decls, Outer_Statements);

         --  Loop through every parameter and get its value from the stream. If
         --  the parameter is unconstrained, then the parameter is read using
         --  'Input at the point of declaration.

         Current_Parameter := First (Ordered_Parameters_List);
         while Present (Current_Parameter) loop
            declare
               Etyp        : Entity_Id;
               Constrained : Boolean;
               Any         : Entity_Id := Empty;
               Object      : constant Entity_Id :=
                               Make_Defining_Identifier (Loc,
                                 New_Internal_Name ('P'));
               Expr        : Node_Id   := Empty;

               Is_Controlling_Formal : constant Boolean
                 := Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type);

               Is_First_Controlling_Formal : Boolean := False;
            begin
               Set_Ekind (Object, E_Variable);

               if Is_Controlling_Formal then

                  --  Controlling formals in distributed object primitive
                  --  operations are handled specially:
                  --    - the first controlling formal is used as the
                  --      target of the call;
                  --    - the remaining controlling formals are transmitted
                  --      as RACWs.

                  Etyp := RACW_Type;
                  Is_First_Controlling_Formal :=
                    not First_Controlling_Formal_Seen;
                  First_Controlling_Formal_Seen := True;
               else
                  Etyp := Etype (Parameter_Type (Current_Parameter));
               end if;

               Constrained :=
                 Is_Constrained (Etyp)
                 or else Is_Elementary_Type (Etyp);

               if not Is_First_Controlling_Formal then
                  Any := Make_Defining_Identifier (Loc,
                           New_Internal_Name ('A'));
                  Append_To (Outer_Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Any,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Any), Loc),
                      Expression =>
                        Make_Function_Call (Loc,
                          Name =>
                            New_Occurrence_Of (RTE (RE_Create_Any), Loc),
                          Parameter_Associations => New_List (
                            PolyORB_Support.Helpers.Build_TypeCode_Call (Loc,
                              Etyp, Outer_Decls)))));

                  Append_To (Outer_Statements,
                    Add_Parameter_To_NVList (Loc,
                      Parameter   => Current_Parameter,
                      NVList      => Arguments,
                      Constrained => Constrained,
                      Any         => Any));
               end if;

               if Is_First_Controlling_Formal then
                  declare
                     Addr : constant Entity_Id :=
                       Make_Defining_Identifier (Loc,
                         New_Internal_Name ('A'));
                     Is_Local : constant Entity_Id :=
                       Make_Defining_Identifier (Loc,
                         New_Internal_Name ('L'));
                  begin

                     --  Special case: obtain the first controlling
                     --  formal from the target of the remote call,
                     --  instead of the argument list.

                     Append_To (Outer_Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier =>
                           Addr,
                         Object_Definition =>
                           New_Occurrence_Of (RTE (RE_Address), Loc)));
                     Append_To (Outer_Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier =>
                           Is_Local,
                         Object_Definition =>
                           New_Occurrence_Of (Standard_Boolean, Loc)));
                     Append_To (Outer_Statements,
                       Make_Procedure_Call_Statement (Loc,
                         Name =>
                           New_Occurrence_Of (
                             RTE (RE_Get_Local_Address), Loc),
                         Parameter_Associations => New_List (
                           Make_Selected_Component (Loc,
                             Prefix =>
                               New_Occurrence_Of (
                                 Request_Parameter, Loc),
                             Selector_Name =>
                               Make_Identifier (Loc, Name_Target)),
                           New_Occurrence_Of (Is_Local, Loc),
                           New_Occurrence_Of (Addr, Loc))));

                     Expr := Unchecked_Convert_To (RACW_Type,
                       New_Occurrence_Of (Addr, Loc));
                  end;

               elsif In_Present (Current_Parameter)
                  or else not Out_Present (Current_Parameter)
                  or else not Constrained
               then
                  --  If an input parameter is contrained, then its reading is
                  --  deferred until the beginning of the subprogram body. If
                  --  it is unconstrained, then an expression is built for
                  --  the object declaration and the variable is set using
                  --  'Input instead of 'Read.

                  Expr := PolyORB_Support.Helpers.Build_From_Any_Call (
                            Etyp, New_Occurrence_Of (Any, Loc), Decls);

                  if Constrained then

                     Append_To (Statements,
                       Make_Assignment_Statement (Loc,
                         Name =>
                           New_Occurrence_Of (Object, Loc),
                         Expression =>
                            Expr));
                     Expr := Empty;
                  else
                     null;
                     --  Expr will be used to initialize (and constrain)
                     --  the parameter when it is declared.
                  end if;

               end if;

               --  If we do not have to output the current parameter, then
               --  it can well be flagged as constant. This may allow further
               --  optimizations done by the back end.

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Object,
                   Constant_Present    => not Constrained
                     and then not Out_Present (Current_Parameter),
                   Object_Definition   =>
                     New_Occurrence_Of (Etyp, Loc),
                   Expression          => Expr));
               Set_Etype (Object, Etyp);

               --  An out parameter may be written back using a 'Write
               --  attribute instead of a 'Output because it has been
               --  constrained by the parameter given to the caller. Note that
               --  out controlling arguments in the case of a RACW are not put
               --  back in the stream because the pointer on them has not
               --  changed.

               if Out_Present (Current_Parameter)
                 and then not Is_Controlling_Formal
               then
                  Append_To (After_Statements,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Copy_Any_Value), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Any, Loc),
                        PolyORB_Support.Helpers.Build_To_Any_Call (
                          New_Occurrence_Of (Object, Loc),
                          Decls))));
               end if;

               --  For RACW controlling formals, the Etyp of Object is always
               --  an RACW, even if the parameter is not of an anonymous access
               --  type. In such case, we need to dereference it at call time.

               if Is_Controlling_Formal then
                  if Nkind (Parameter_Type (Current_Parameter)) /=
                    N_Access_Definition
                  then
                     Append_To (Parameter_List,
                       Make_Parameter_Association (Loc,
                         Selector_Name             =>
                           New_Occurrence_Of (
                             Defining_Identifier (Current_Parameter), Loc),
                         Explicit_Actual_Parameter =>
                           Make_Explicit_Dereference (Loc,
                             Unchecked_Convert_To (RACW_Type,
                               OK_Convert_To (RTE (RE_Address),
                                 New_Occurrence_Of (Object, Loc))))));

                  else
                     Append_To (Parameter_List,
                       Make_Parameter_Association (Loc,
                         Selector_Name             =>
                           New_Occurrence_Of (
                             Defining_Identifier (Current_Parameter), Loc),
                         Explicit_Actual_Parameter =>
                           Unchecked_Convert_To (RACW_Type,
                             OK_Convert_To (RTE (RE_Address),
                               New_Occurrence_Of (Object, Loc)))));
                  end if;

               else
                  Append_To (Parameter_List,
                    Make_Parameter_Association (Loc,
                      Selector_Name             =>
                        New_Occurrence_Of (
                          Defining_Identifier (Current_Parameter), Loc),
                      Explicit_Actual_Parameter =>
                        New_Occurrence_Of (Object, Loc)));
               end if;

               --  If the current parameter needs an extra formal, then read it
               --  from the stream and set the corresponding semantic field in
               --  the variable. If the kind of the parameter identifier is
               --  E_Void, then this is a compiler generated parameter that
               --  doesn't need an extra constrained status.

               --  The case of Extra_Accessibility should also be handled ???

               if Nkind (Parameter_Type (Current_Parameter)) /=
                                                         N_Access_Definition
                 and then
                   Ekind (Defining_Identifier (Current_Parameter)) /= E_Void
                 and then
                   Present (Extra_Constrained
                     (Defining_Identifier (Current_Parameter)))
               then
                  declare
                     Extra_Parameter : constant Entity_Id :=
                                         Extra_Constrained
                                           (Defining_Identifier
                                             (Current_Parameter));
                     Extra_Any : constant Entity_Id :=
                       Make_Defining_Identifier
                         (Loc, New_Internal_Name ('A'));
                     Formal_Entity : constant Entity_Id :=
                                       Make_Defining_Identifier
                                           (Loc, Chars (Extra_Parameter));

                     Formal_Type : constant Entity_Id :=
                                     Etype (Extra_Parameter);
                  begin
                     Append_To (Outer_Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier =>
                           Extra_Any,
                         Object_Definition   =>
                           New_Occurrence_Of (RTE (RE_Any), Loc)));

                     Append_To (Outer_Statements,
                       Add_Parameter_To_NVList (Loc,
                         Parameter   => Extra_Parameter,
                         NVList      => Arguments,
                         Constrained => True,
                         Any         => Extra_Any));

                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Formal_Entity,
                         Object_Definition   =>
                           New_Occurrence_Of (Formal_Type, Loc)));

                     Append_To (Extra_Formal_Statements,
                       Make_Assignment_Statement (Loc,
                         Name =>
                           New_Occurrence_Of (Extra_Parameter, Loc),
                         Expression =>
                           PolyORB_Support.Helpers.Build_From_Any_Call (
                             Etype (Extra_Parameter),
                             New_Occurrence_Of (Extra_Any, Loc),
                       Decls)));
                     Set_Extra_Constrained (Object, Formal_Entity);

                  end;
               end if;
            end;

            Next (Current_Parameter);
         end loop;

         Append_To (Outer_Statements,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Request_Arguments), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Request_Parameter, Loc),
               New_Occurrence_Of (Arguments, Loc))));

         Append_List_To (Statements, Extra_Formal_Statements);

         if Nkind (Specification (Vis_Decl)) = N_Function_Specification then

            --  The remote subprogram is a function. We build an inner block to
            --  be able to hold a potentially unconstrained result in a
            --  variable.

            declare
               Etyp   : constant Entity_Id :=
                          Etype (Subtype_Mark (Specification (Vis_Decl)));
               Result : constant Node_Id   :=
                          Make_Defining_Identifier (Loc,
                            New_Internal_Name ('R'));
            begin
               Inner_Decls := New_List (
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Result,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Etyp, Loc),
                   Expression          =>
                     Make_Function_Call (Loc,
                       Name                   => Called_Subprogram,
                       Parameter_Associations => Parameter_List)));

               Set_Etype (Result, Etyp);
               Append_To (After_Statements,
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_Set_Result), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Request_Parameter, Loc),
                     PolyORB_Support.Helpers.Build_To_Any_Call (
                       New_Occurrence_Of (Result, Loc),
                       Decls))));
               --  A DSA function does not have out or inout arguments
            end;

            Append_To (Statements,
              Make_Block_Statement (Loc,
                Declarations               => Inner_Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => After_Statements)));

         else
            --  The remote subprogram is a procedure. We do not need any inner
            --  block in this case. No specific processing is required here for
            --  the dynamically asynchronous case: the indication of whether
            --  call is asynchronous or not is managed by the Sync_Scope
            --  attibute of the request, and is handled entirely in the
            --  protocol layer.

            Append_To (After_Statements,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Request_Set_Out), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Request_Parameter, Loc))));

            Append_To (Statements,
              Make_Procedure_Call_Statement (Loc,
                Name                   => Called_Subprogram,
                Parameter_Associations => Parameter_List));

            Append_List_To (Statements, After_Statements);
         end if;

         Subp_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       =>
               Make_Defining_Identifier (Loc, New_Internal_Name ('F')),

             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Request_Parameter,
                 Parameter_Type      =>
                   New_Occurrence_Of (RTE (RE_Request_Access), Loc))));

         --  An exception raised during the execution of an incoming
         --  remote subprogram call and that needs to be sent back
         --  to the caller is propagated by the receiving stubs, and
         --  will be handled by the caller (the distribution runtime).

         if Asynchronous and then not Dynamically_Asynchronous then

            --  For an asynchronous procedure, add a null exception handler

            Excep_Handlers := New_List (
              Make_Exception_Handler (Loc,
                Exception_Choices => New_List (Make_Others_Choice (Loc)),
                Statements        => New_List (Make_Null_Statement (Loc))));

         else

            --  In the other cases, if an exception is raised, then the
            --  exception occurrence is propagated.

            null;
         end if;

         Append_To (Outer_Statements,
           Make_Block_Statement (Loc,
             Declarations =>
               Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Statements)));

         return
           Make_Subprogram_Body (Loc,
             Specification              => Subp_Spec,
             Declarations               => Outer_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements         => Outer_Statements,
                 Exception_Handlers => Excep_Handlers));
      end Build_Subprogram_Receiving_Stubs;
      -------------
      -- Helpers --
      -------------

      package body Helpers is

         -----------------------
         -- Local Subprograms --
         -----------------------

         function Find_Numeric_Representation
           (Typ : Entity_Id) return Entity_Id;
         --  Given a numeric type Typ, return the smallest integer or floarting
         --  point type from Standard, or the smallest unsigned (modular) type
         --  from System.Unsigned_Types, whose range encompasses that of Typ.

         function Make_Stream_Procedure_Function_Name
           (Loc : Source_Ptr;
            Typ : Entity_Id;
            Nam : Name_Id) return Entity_Id;
         --  Return the name to be assigned for stream subprogram Nam of Typ.
         --  (copied from exp_strm.adb, should be shared???)

         ------------------------------------------------------------
         -- Common subprograms for building various tree fragments --
         ------------------------------------------------------------

         function Build_Get_Aggregate_Element
           (Loc : Source_Ptr;
            Any : Entity_Id;
            TC  : Node_Id;
            Idx : Node_Id) return Node_Id;
         --  Build a call to Get_Aggregate_Element on Any
         --  for typecode TC, returning the Idx'th element.

         generic
            Subprogram : Entity_Id;
            --  Reference location for constructed nodes

            Arry : Entity_Id;
            --  For 'Range and Etype

            Indices : List_Id;
            --  For the construction of the innermost element expression

            with procedure Add_Process_Element
              (Stmts   : List_Id;
               Any     : Entity_Id;
               Counter : Entity_Id;
               Datum   : Node_Id);

         procedure Append_Array_Traversal
           (Stmts   : List_Id;
            Any     : Entity_Id;
            Counter : Entity_Id := Empty;
            Depth   : Pos       := 1);
         --  Build nested loop statements that iterate over the elements of an
         --  array Arry. The statement(s) built by Add_Process_Element are
         --  executed for each element; Indices is the list of indices to be
         --  used in the construction of the indexed component that denotes the
         --  current element. Subprogram is the entity for the subprogram for
         --  which this iterator is generated. The generated statements are
         --  appended to Stmts.

         generic
            Rec : Entity_Id;
            --  The record entity being dealt with

            with procedure Add_Process_Element
              (Stmts     : List_Id;
               Container : Node_Or_Entity_Id;
               Counter   : in out Int;
               Rec       : Entity_Id;
               Field     : Node_Id);
            --  Rec is the instance of the record type, or Empty.
            --  Field is either the N_Defining_Identifier for a component,
            --  or an N_Variant_Part.

         procedure Append_Record_Traversal
           (Stmts     : List_Id;
            Clist     : Node_Id;
            Container : Node_Or_Entity_Id;
            Counter   : in out Int);
         --  Process component list Clist. Individual fields are passed
         --  to Field_Processing. Each variant part is also processed.
         --  Container is the outer Any (for From_Any/To_Any),
         --  the outer typecode (for TC) to which the operation applies.

         -----------------------------
         -- Append_Record_Traversal --
         -----------------------------

         procedure Append_Record_Traversal
           (Stmts     : List_Id;
            Clist     : Node_Id;
            Container : Node_Or_Entity_Id;
            Counter   : in out Int)
         is
            CI : constant List_Id := Component_Items (Clist);
            VP : constant Node_Id := Variant_Part (Clist);

            Item : Node_Id := First (CI);
            Def  : Entity_Id;

         begin
            while Present (Item) loop
               Def := Defining_Identifier (Item);
               if not Is_Internal_Name (Chars (Def)) then
                  Add_Process_Element
                    (Stmts, Container, Counter, Rec, Def);
               end if;
               Next (Item);
            end loop;

            if Present (VP) then
               Add_Process_Element (Stmts, Container, Counter, Rec, VP);
            end if;
         end Append_Record_Traversal;

         -------------------------
         -- Build_From_Any_Call --
         -------------------------

         function Build_From_Any_Call
           (Typ   : Entity_Id;
            N     : Node_Id;
            Decls : List_Id) return Node_Id
         is
            Loc : constant Source_Ptr := Sloc (N);

            U_Type : Entity_Id  := Underlying_Type (Typ);

            Fnam    : Entity_Id := Empty;
            Lib_RE  : RE_Id := RE_Null;

         begin

            --  First simple case where the From_Any function is present
            --  in the type's TSS.

            Fnam := Find_Inherited_TSS (U_Type, TSS_From_Any);

            if Sloc (U_Type) <= Standard_Location then
               U_Type := Base_Type (U_Type);
            end if;

            --  Check first for Boolean and Character. These are enumeration
            --  types, but we treat them specially, since they may require
            --  special handling in the transfer protocol. However, this
            --  special handling only applies if they have standard
            --  representation, otherwise they are treated like any other
            --  enumeration type.

            if Present (Fnam) then
               null;

            elsif U_Type = Standard_Boolean then
               Lib_RE := RE_FA_B;

            elsif U_Type = Standard_Character then
               Lib_RE := RE_FA_C;

            elsif U_Type = Standard_Wide_Character then
               Lib_RE := RE_FA_WC;

            elsif U_Type = Standard_Wide_Wide_Character then
               Lib_RE := RE_FA_WWC;

            --  Floating point types

            elsif U_Type = Standard_Short_Float then
               Lib_RE := RE_FA_SF;

            elsif U_Type = Standard_Float then
               Lib_RE := RE_FA_F;

            elsif U_Type = Standard_Long_Float then
               Lib_RE := RE_FA_LF;

            elsif U_Type = Standard_Long_Long_Float then
               Lib_RE := RE_FA_LLF;

            --  Integer types

            elsif U_Type = Etype (Standard_Short_Short_Integer) then
                  Lib_RE := RE_FA_SSI;

            elsif U_Type = Etype (Standard_Short_Integer) then
               Lib_RE := RE_FA_SI;

            elsif U_Type = Etype (Standard_Integer) then
               Lib_RE := RE_FA_I;

            elsif U_Type = Etype (Standard_Long_Integer) then
               Lib_RE := RE_FA_LI;

            elsif U_Type = Etype (Standard_Long_Long_Integer) then
               Lib_RE := RE_FA_LLI;

            --  Unsigned integer types

            elsif U_Type = RTE (RE_Short_Short_Unsigned) then
               Lib_RE := RE_FA_SSU;

            elsif U_Type = RTE (RE_Short_Unsigned) then
               Lib_RE := RE_FA_SU;

            elsif U_Type = RTE (RE_Unsigned) then
               Lib_RE := RE_FA_U;

            elsif U_Type = RTE (RE_Long_Unsigned) then
               Lib_RE := RE_FA_LU;

            elsif U_Type = RTE (RE_Long_Long_Unsigned) then
               Lib_RE := RE_FA_LLU;

            elsif U_Type = Standard_String then
               Lib_RE := RE_FA_String;

            --  Other (non-primitive) types

            else
               declare
                  Decl : Entity_Id;
               begin
                  Build_From_Any_Function (Loc, U_Type, Decl, Fnam);
                  Append_To (Decls, Decl);
               end;
            end if;

            --  Call the function

            if Lib_RE /= RE_Null then
               pragma Assert (No (Fnam));
               Fnam := RTE (Lib_RE);
            end if;

            return
                Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (Fnam, Loc),
                  Parameter_Associations => New_List (N));
         end Build_From_Any_Call;

         -----------------------------
         -- Build_From_Any_Function --
         -----------------------------

         procedure Build_From_Any_Function
           (Loc  : Source_Ptr;
            Typ  : Entity_Id;
            Decl : out Node_Id;
            Fnam : out Entity_Id)
         is
            Spec : Node_Id;
            Decls : constant List_Id := New_List;
            Stms : constant List_Id := New_List;
            Any_Parameter : constant Entity_Id
              := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
         begin
            Fnam := Make_Stream_Procedure_Function_Name (Loc,
                      Typ, Name_uFrom_Any);

            Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name => Fnam,
                Parameter_Specifications => New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Any_Parameter,
                    Parameter_Type =>
                      New_Occurrence_Of (RTE (RE_Any), Loc))),
                Subtype_Mark => New_Occurrence_Of (Typ, Loc));

            --  The following  is taken care of by Exp_Dist.Add_RACW_From_Any

            pragma Assert
              (not (Is_Remote_Access_To_Class_Wide_Type (Typ)));

            if Is_Derived_Type (Typ)
              and then not Is_Tagged_Type (Typ)
            then
               Append_To (Stms,
                 Make_Return_Statement (Loc,
                   Expression =>
                     OK_Convert_To (
                       Typ,
                       Build_From_Any_Call (
                         Root_Type (Typ),
                         New_Occurrence_Of (Any_Parameter, Loc),
                         Decls))));

            elsif Is_Record_Type (Typ)
              and then not Is_Derived_Type (Typ)
              and then not Is_Tagged_Type (Typ)
            then
               if Nkind (Declaration_Node (Typ)) = N_Subtype_Declaration then
                  Append_To (Stms,
                    Make_Return_Statement (Loc,
                      Expression =>
                        OK_Convert_To (
                          Typ,
                          Build_From_Any_Call (
                            Etype (Typ),
                            New_Occurrence_Of (Any_Parameter, Loc),
                            Decls))));
               else
                  declare
                     Disc : Entity_Id := Empty;
                     Discriminant_Associations : List_Id;
                     Rdef : constant Node_Id :=
                       Type_Definition (Declaration_Node (Typ));
                     Component_Counter : Int := 0;

                     --  The returned object

                     Res : constant Entity_Id :=
                             Make_Defining_Identifier (Loc,
                               New_Internal_Name ('R'));

                     Res_Definition : Node_Id := New_Occurrence_Of (Typ, Loc);

                     procedure FA_Rec_Add_Process_Element
                       (Stmts   : List_Id;
                        Any     : Entity_Id;
                        Counter : in out Int;
                        Rec     : Entity_Id;
                        Field   : Node_Id);

                     procedure FA_Append_Record_Traversal is
                        new Append_Record_Traversal
                       (Rec                 => Res,
                        Add_Process_Element => FA_Rec_Add_Process_Element);

                     --------------------------------
                     -- FA_Rec_Add_Process_Element --
                     --------------------------------

                     procedure FA_Rec_Add_Process_Element
                       (Stmts   : List_Id;
                        Any     : Entity_Id;
                        Counter : in out Int;
                        Rec     : Entity_Id;
                        Field   : Node_Id)
                     is
                     begin
                        if Nkind (Field) = N_Defining_Identifier then

                           --  A regular component

                           Append_To (Stmts,
                             Make_Assignment_Statement (Loc,
                               Name => Make_Selected_Component (Loc,
                                 Prefix        =>
                                   New_Occurrence_Of (Rec, Loc),
                                 Selector_Name =>
                                   New_Occurrence_Of (Field, Loc)),
                               Expression =>
                                 Build_From_Any_Call (Etype (Field),
                                   Build_Get_Aggregate_Element (Loc,
                                     Any => Any,
                                     Tc  => Build_TypeCode_Call (Loc,
                                              Etype (Field), Decls),
                                     Idx => Make_Integer_Literal (Loc,
                                              Counter)),
                                   Decls)));

                        else
                           --  A variant part

                           declare
                              Variant : Node_Id;
                              Struct_Counter : Int := 0;

                              Block_Decls : constant List_Id := New_List;
                              Block_Stmts : constant List_Id := New_List;
                              VP_Stmts    : List_Id;

                              Alt_List    : constant List_Id := New_List;
                              Choice_List : List_Id;

                              Struct_Any : constant Entity_Id :=
                                             Make_Defining_Identifier (Loc,
                                               New_Internal_Name ('S'));

                           begin
                              Append_To (Decls,
                                Make_Object_Declaration (Loc,
                                  Defining_Identifier =>
                                    Struct_Any,
                                  Constant_Present =>
                                     True,
                                  Object_Definition =>
                                     New_Occurrence_Of (RTE (RE_Any), Loc),
                                  Expression =>
                                    Make_Function_Call (Loc,
                                      Name => New_Occurrence_Of (
                                        RTE (RE_Extract_Union_Value), Loc),
                                      Parameter_Associations => New_List (
                                        Build_Get_Aggregate_Element (Loc,
                                          Any => Any,
                                          Tc  => Make_Function_Call (Loc,
                                            Name => New_Occurrence_Of (
                                              RTE (RE_Any_Member_Type), Loc),
                                            Parameter_Associations =>
                                              New_List (
                                                New_Occurrence_Of (Any, Loc),
                                                Make_Integer_Literal (Loc,
                                                  Counter))),
                                          Idx => Make_Integer_Literal (Loc,
                                            Counter))))));

                              Append_To (Stmts,
                                Make_Block_Statement (Loc,
                                  Declarations =>
                                    Block_Decls,
                                  Handled_Statement_Sequence =>
                                    Make_Handled_Sequence_Of_Statements (Loc,
                                      Statements => Block_Stmts)));

                              Append_To (Block_Stmts,
                                Make_Case_Statement (Loc,
                                    Expression =>
                                      Make_Selected_Component (Loc,
                                        Prefix        => Rec,
                                        Selector_Name =>
                                          Chars (Name (Field))),
                                    Alternatives =>
                                      Alt_List));

                              Variant := First_Non_Pragma (Variants (Field));

                              while Present (Variant) loop
                                 Choice_List := New_Copy_List_Tree
                                   (Discrete_Choices (Variant));

                                 VP_Stmts := New_List;
                                 FA_Append_Record_Traversal (
                                   Stmts     => VP_Stmts,
                                   Clist     => Component_List (Variant),
                                   Container => Struct_Any,
                                   Counter   => Struct_Counter);

                                 Append_To (Alt_List,
                                   Make_Case_Statement_Alternative (Loc,
                                     Discrete_Choices => Choice_List,
                                     Statements =>
                                       VP_Stmts));
                                 Next_Non_Pragma (Variant);
                              end loop;
                           end;
                        end if;
                        Counter := Counter + 1;
                     end FA_Rec_Add_Process_Element;

                  begin
                     --  First all discriminants

                     if Has_Discriminants (Typ) then
                        Disc := First_Discriminant (Typ);
                        Discriminant_Associations := New_List;

                        while Present (Disc) loop
                           declare
                              Disc_Var_Name : constant Entity_Id :=
                                Make_Defining_Identifier (Loc, Chars (Disc));
                              Disc_Type : constant Entity_Id :=
                                Etype (Disc);
                           begin
                              Append_To (Decls,
                                Make_Object_Declaration (Loc,
                                  Defining_Identifier =>
                                    Disc_Var_Name,
                                  Constant_Present => True,
                                  Object_Definition =>
                                    New_Occurrence_Of (Disc_Type, Loc),
                                  Expression =>
                                    Build_From_Any_Call (Etype (Disc),
                                      Build_Get_Aggregate_Element (Loc,
                                        Any => Any_Parameter,
                                        Tc  => Build_TypeCode_Call
                                                 (Loc, Etype (Disc), Decls),
                                        Idx => Make_Integer_Literal
                                                 (Loc, Component_Counter)),
                                      Decls)));
                              Component_Counter := Component_Counter + 1;

                              Append_To (Discriminant_Associations,
                                Make_Discriminant_Association (Loc,
                                  Selector_Names => New_List (
                                    New_Occurrence_Of (Disc, Loc)),
                                  Expression =>
                                    New_Occurrence_Of (Disc_Var_Name, Loc)));
                           end;
                           Next_Discriminant (Disc);
                        end loop;

                        Res_Definition := Make_Subtype_Indication (Loc,
                          Subtype_Mark => Res_Definition,
                          Constraint   =>
                            Make_Index_Or_Discriminant_Constraint (Loc,
                              Discriminant_Associations));
                     end if;

                     --  Now we have all the discriminants in variables, we can
                     --  declared a constrained object. Note that we are not
                     --  initializing (non-discriminant) components directly in
                     --  the object declarations, because which fields to
                     --  initialize depends (at run time) on the discriminant
                     --  values.

                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier =>
                           Res,
                         Object_Definition =>
                           Res_Definition));

                     --  ... then all components

                     FA_Append_Record_Traversal (Stms,
                       Clist     => Component_List (Rdef),
                       Container => Any_Parameter,
                       Counter   => Component_Counter);

                     Append_To (Stms,
                       Make_Return_Statement (Loc,
                         Expression => New_Occurrence_Of (Res, Loc)));
                  end;
               end if;

            elsif Is_Array_Type (Typ) then
               declare
                  Constrained : constant Boolean := Is_Constrained (Typ);

                  procedure FA_Ary_Add_Process_Element
                    (Stmts   : List_Id;
                     Any     : Entity_Id;
                     Counter : Entity_Id;
                     Datum   : Node_Id);
                  --  Assign the current element (as identified by Counter) of
                  --  Any to the variable denoted by name Datum, and advance
                  --  Counter by 1. If Datum is not an Any, a call to From_Any
                  --  for its type is inserted.

                  --------------------------------
                  -- FA_Ary_Add_Process_Element --
                  --------------------------------

                  procedure FA_Ary_Add_Process_Element
                    (Stmts   : List_Id;
                     Any     : Entity_Id;
                     Counter : Entity_Id;
                     Datum   : Node_Id)
                  is
                     Assignment : constant Node_Id :=
                       Make_Assignment_Statement (Loc,
                         Name       => Datum,
                         Expression => Empty);

                     Element_Any : constant Node_Id :=
                       Build_Get_Aggregate_Element (Loc,
                         Any => Any,
                         Tc  => Build_TypeCode_Call (Loc,
                                  Etype (Datum), Decls),
                         Idx => New_Occurrence_Of (Counter, Loc));

                  begin
                     --  Note: here we *prepend* statements to Stmts, so
                     --  we must do it in reverse order.

                     Prepend_To (Stmts,
                       Make_Assignment_Statement (Loc,
                         Name =>
                           New_Occurrence_Of (Counter, Loc),
                         Expression =>
                           Make_Op_Add (Loc,
                             Left_Opnd =>
                               New_Occurrence_Of (Counter, Loc),
                             Right_Opnd =>
                               Make_Integer_Literal (Loc, 1))));

                     if Nkind (Datum) /= N_Attribute_Reference then

                        --  We ignore the value of the length of each
                        --  dimension, since the target array has already
                        --  been constrained anyway.

                        if Etype (Datum) /= RTE (RE_Any) then
                           Set_Expression (Assignment,
                              Build_From_Any_Call (
                                Component_Type (Typ),
                                Element_Any,
                                Decls));
                        else
                           Set_Expression (Assignment, Element_Any);
                        end if;
                        Prepend_To (Stmts, Assignment);
                     end if;
                  end FA_Ary_Add_Process_Element;

                  Counter : constant Entity_Id :=
                              Make_Defining_Identifier (Loc, Name_J);

                  Initial_Counter_Value : Int := 0;

                  Component_TC : constant Entity_Id :=
                                   Make_Defining_Identifier (Loc, Name_T);

                  Res : constant Entity_Id :=
                          Make_Defining_Identifier (Loc, Name_R);

                  procedure Append_From_Any_Array_Iterator is
                    new Append_Array_Traversal (
                      Subprogram => Fnam,
                      Arry       => Res,
                      Indices    => New_List,
                      Add_Process_Element => FA_Ary_Add_Process_Element);

                  Res_Subtype_Indication : Node_Id :=
                                             New_Occurrence_Of (Typ, Loc);

               begin
                  if not Constrained then
                     declare
                        Ndim : constant Int := Number_Dimensions (Typ);
                        Lnam : Name_Id;
                        Hnam : Name_Id;
                        Indx : Node_Id := First_Index (Typ);
                        Indt : Entity_Id;

                        Ranges : constant List_Id := New_List;

                     begin
                        for J in 1 .. Ndim loop
                           Lnam := New_External_Name ('L', J);
                           Hnam := New_External_Name ('H', J);
                           Indt := Etype (Indx);

                           Append_To (Decls,
                             Make_Object_Declaration (Loc,
                               Defining_Identifier =>
                                 Make_Defining_Identifier (Loc, Lnam),
                               Constant_Present    =>
                                 True,
                               Object_Definition   =>
                                 New_Occurrence_Of (Indt, Loc),
                               Expression          =>
                                 Build_From_Any_Call (
                                   Indt,
                                   Build_Get_Aggregate_Element (Loc,
                                     Any => Any_Parameter,
                                     Tc  => Build_TypeCode_Call (Loc,
                                              Indt, Decls),
                                     Idx => Make_Integer_Literal (Loc, J - 1)),
                                   Decls)));

                           Append_To (Decls,
                             Make_Object_Declaration (Loc,
                               Defining_Identifier =>
                                 Make_Defining_Identifier (Loc, Hnam),
                               Constant_Present =>
                                 True,
                               Object_Definition =>
                                 New_Occurrence_Of (Indt, Loc),
                               Expression => Make_Attribute_Reference (Loc,
                                 Prefix         =>
                                   New_Occurrence_Of (Indt, Loc),
                                 Attribute_Name => Name_Val,
                                 Expressions    => New_List (
                                   Make_Op_Subtract (Loc,
                                     Left_Opnd =>
                                       Make_Op_Add (Loc,
                                         Left_Opnd =>
                                           Make_Attribute_Reference (Loc,
                                             Prefix         =>
                                               New_Occurrence_Of (Indt, Loc),
                                             Attribute_Name =>
                                               Name_Pos,
                                             Expressions    => New_List (
                                               Make_Identifier (Loc, Lnam))),
                                         Right_Opnd =>
                                           Make_Function_Call (Loc,
                                             Name => New_Occurrence_Of (RTE (
                                               RE_Get_Nested_Sequence_Length),
                                               Loc),
                                             Parameter_Associations =>
                                               New_List (
                                                 New_Occurrence_Of (
                                                   Any_Parameter, Loc),
                                                 Make_Integer_Literal (Loc,
                                                   J)))),
                                     Right_Opnd =>
                                       Make_Integer_Literal (Loc, 1))))));

                           Append_To (Ranges,
                             Make_Range (Loc,
                               Low_Bound  => Make_Identifier (Loc, Lnam),
                               High_Bound => Make_Identifier (Loc, Hnam)));

                           Next_Index (Indx);
                        end loop;

                        --  Now we have all the necessary bound information:
                        --  apply the set of range constraints to the
                        --  (unconstrained) nominal subtype of Res.

                        Initial_Counter_Value := Ndim;
                        Res_Subtype_Indication := Make_Subtype_Indication (Loc,
                          Subtype_Mark =>
                            Res_Subtype_Indication,
                          Constraint   =>
                            Make_Index_Or_Discriminant_Constraint (Loc,
                              Constraints => Ranges));
                     end;
                  end if;

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Res,
                      Object_Definition => Res_Subtype_Indication));
                  Set_Etype (Res, Typ);

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Counter,
                      Object_Definition =>
                        New_Occurrence_Of (RTE (RE_Long_Unsigned), Loc),
                      Expression =>
                        Make_Integer_Literal (Loc, Initial_Counter_Value)));

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Component_TC,
                      Constant_Present => True,
                      Object_Definition =>
                        New_Occurrence_Of (RTE (RE_TypeCode), Loc),
                      Expression =>
                        Build_TypeCode_Call (Loc,
                          Component_Type (Typ), Decls)));

                  Append_From_Any_Array_Iterator (Stms,
                    Any_Parameter, Counter);

                  Append_To (Stms,
                    Make_Return_Statement (Loc,
                      Expression => New_Occurrence_Of (Res, Loc)));
               end;

            elsif Is_Integer_Type (Typ) or else Is_Unsigned_Type (Typ) then
               Append_To (Stms,
                 Make_Return_Statement (Loc,
                   Expression =>
                     Unchecked_Convert_To (
                       Typ,
                       Build_From_Any_Call (
                         Find_Numeric_Representation (Typ),
                         New_Occurrence_Of (Any_Parameter, Loc),
                         Decls))));

            else
               --  Default: type is represented as an opaque sequence of bytes

               declare
                  Strm : constant Entity_Id :=
                           Make_Defining_Identifier (Loc,
                             Chars => New_Internal_Name ('S'));
                  Res  : constant Entity_Id :=
                           Make_Defining_Identifier (Loc,
                             Chars => New_Internal_Name ('R'));

               begin
                  --  Strm : Buffer_Stream_Type;

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Strm,
                      Aliased_Present     =>
                        True,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Buffer_Stream_Type), Loc)));

                  --  Any_To_BS (Strm, A);

                  Append_To (Stms,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Any_To_BS), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Any_Parameter, Loc),
                        New_Occurrence_Of (Strm, Loc))));

                  --  declare
                  --     Res : constant T := T'Input (Strm);
                  --  begin
                  --     Release_Buffer (Strm);
                  --     return Res;
                  --  end;

                  Append_To (Stms, Make_Block_Statement (Loc,
                    Declarations => New_List (
                      Make_Object_Declaration (Loc,
                        Defining_Identifier => Res,
                        Constant_Present    => True,
                        Object_Definition   =>
                          New_Occurrence_Of (Typ, Loc),
                        Expression          =>
                            Make_Attribute_Reference (Loc,
                              Prefix         => New_Occurrence_Of (Typ, Loc),
                              Attribute_Name => Name_Input,
                              Expressions => New_List (
                                Make_Attribute_Reference (Loc,
                                  Prefix => New_Occurrence_Of (Strm, Loc),
                                  Attribute_Name => Name_Access))))),

                    Handled_Statement_Sequence =>
                      Make_Handled_Sequence_Of_Statements (Loc,
                        Statements => New_List (
                          Make_Procedure_Call_Statement (Loc,
                            Name =>
                              New_Occurrence_Of (RTE (RE_Release_Buffer), Loc),
                            Parameter_Associations =>
                              New_List (
                                New_Occurrence_Of (Strm, Loc))),
                          Make_Return_Statement (Loc,
                            Expression => New_Occurrence_Of (Res, Loc))))));

               end;
            end if;

            Decl :=
              Make_Subprogram_Body (Loc,
                Specification => Spec,
                Declarations => Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Stms));
         end Build_From_Any_Function;

         ---------------------------------
         -- Build_Get_Aggregate_Element --
         ---------------------------------

         function Build_Get_Aggregate_Element
           (Loc : Source_Ptr;
            Any : Entity_Id;
            TC  : Node_Id;
            Idx : Node_Id) return Node_Id
         is
         begin
            return Make_Function_Call (Loc,
              Name =>
                New_Occurrence_Of (
                  RTE (RE_Get_Aggregate_Element), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Any, Loc),
                TC,
                Idx));
         end Build_Get_Aggregate_Element;

         -------------------------
         -- Build_Reposiroty_Id --
         -------------------------

         procedure Build_Name_And_Repository_Id
           (E           : Entity_Id;
            Name_Str    : out String_Id;
            Repo_Id_Str : out String_Id)
         is
         begin
            Start_String;
            Store_String_Chars ("DSA:");
            Get_Library_Unit_Name_String (Scope (E));
            Store_String_Chars (
              Name_Buffer (Name_Buffer'First
                .. Name_Buffer'First + Name_Len - 1));
            Store_String_Char ('.');
            Get_Name_String (Chars (E));
            Store_String_Chars (
              Name_Buffer (Name_Buffer'First
                .. Name_Buffer'First + Name_Len - 1));
            Store_String_Chars (":1.0");
            Repo_Id_Str := End_String;
            Name_Str    := String_From_Name_Buffer;
         end Build_Name_And_Repository_Id;

         -----------------------
         -- Build_To_Any_Call --
         -----------------------

         function Build_To_Any_Call
           (N     : Node_Id;
            Decls : List_Id) return Node_Id
         is
            Loc : constant Source_Ptr := Sloc (N);

            Typ     : Entity_Id := Etype (N);
            U_Type  : Entity_Id;

            Fnam    : Entity_Id := Empty;
            Lib_RE  : RE_Id := RE_Null;

         begin
            --  If N is a selected component, then maybe its Etype
            --  has not been set yet: try to use the Etype of the
            --  selector_name in that case.

            if No (Typ) and then Nkind (N) = N_Selected_Component then
               Typ := Etype (Selector_Name (N));
            end if;
            pragma Assert (Present (Typ));

            --  The full view, if Typ is private; the completion,
            --  if Typ is incomplete.

            U_Type := Underlying_Type (Typ);

            --  First simple case where the To_Any function is present
            --  in the type's TSS.

            Fnam := Find_Inherited_TSS (U_Type, TSS_To_Any);

            --  Check first for Boolean and Character. These are enumeration
            --  types, but we treat them specially, since they may require
            --  special handling in the transfer protocol. However, this
            --  special handling only applies if they have standard
            --  representation, otherwise they are treated like any other
            --  enumeration type.

            if Sloc (U_Type) <= Standard_Location then
               U_Type := Base_Type (U_Type);
            end if;

            if Present (Fnam) then
               null;

            elsif U_Type = Standard_Boolean then
               Lib_RE := RE_TA_B;

            elsif U_Type = Standard_Character then
               Lib_RE := RE_TA_C;

            elsif U_Type = Standard_Wide_Character then
               Lib_RE := RE_TA_WC;

            elsif U_Type = Standard_Wide_Wide_Character then
               Lib_RE := RE_TA_WWC;

            --  Floating point types

            elsif U_Type = Standard_Short_Float then
               Lib_RE := RE_TA_SF;

            elsif U_Type = Standard_Float then
               Lib_RE := RE_TA_F;

            elsif U_Type = Standard_Long_Float then
               Lib_RE := RE_TA_LF;

            elsif U_Type = Standard_Long_Long_Float then
               Lib_RE := RE_TA_LLF;

            --  Integer types

            elsif U_Type = Etype (Standard_Short_Short_Integer) then
                  Lib_RE := RE_TA_SSI;

            elsif U_Type = Etype (Standard_Short_Integer) then
               Lib_RE := RE_TA_SI;

            elsif U_Type = Etype (Standard_Integer) then
               Lib_RE := RE_TA_I;

            elsif U_Type = Etype (Standard_Long_Integer) then
               Lib_RE := RE_TA_LI;

            elsif U_Type = Etype (Standard_Long_Long_Integer) then
               Lib_RE := RE_TA_LLI;

            --  Unsigned integer types

            elsif U_Type = RTE (RE_Short_Short_Unsigned) then
               Lib_RE := RE_TA_SSU;

            elsif U_Type = RTE (RE_Short_Unsigned) then
               Lib_RE := RE_TA_SU;

            elsif U_Type = RTE (RE_Unsigned) then
               Lib_RE := RE_TA_U;

            elsif U_Type = RTE (RE_Long_Unsigned) then
               Lib_RE := RE_TA_LU;

            elsif U_Type = RTE (RE_Long_Long_Unsigned) then
               Lib_RE := RE_TA_LLU;

            elsif U_Type = Standard_String then
               Lib_RE := RE_TA_String;

            elsif U_Type = Underlying_Type (RTE (RE_TypeCode)) then
               Lib_RE := RE_TA_TC;

            --  Other (non-primitive) types

            else
               declare
                  Decl : Entity_Id;
               begin
                  Build_To_Any_Function (Loc, U_Type, Decl, Fnam);
                  Append_To (Decls, Decl);
               end;
            end if;

            --  Call the function

            if Lib_RE /= RE_Null then
               pragma Assert (No (Fnam));
               Fnam := RTE (Lib_RE);
            end if;

            return
                Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (Fnam, Loc),
                  Parameter_Associations => New_List (N));
         end Build_To_Any_Call;

         ---------------------------
         -- Build_To_Any_Function --
         ---------------------------

         procedure Build_To_Any_Function
           (Loc  : Source_Ptr;
            Typ  : Entity_Id;
            Decl : out Node_Id;
            Fnam : out Entity_Id)
         is
            Spec  : Node_Id;
            Decls : constant List_Id := New_List;
            Stms  : constant List_Id := New_List;

            Expr_Parameter : constant Entity_Id :=
                               Make_Defining_Identifier (Loc, Name_E);

            Any : constant Entity_Id :=
                    Make_Defining_Identifier (Loc, Name_A);

            Any_Decl  : Node_Id;
            Result_TC : Node_Id := Build_TypeCode_Call (Loc, Typ, Decls);

         begin
            Fnam := Make_Stream_Procedure_Function_Name (Loc,
                      Typ, Name_uTo_Any);

            Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name => Fnam,
                Parameter_Specifications => New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Expr_Parameter,
                    Parameter_Type =>
                      New_Occurrence_Of (Typ, Loc))),
                Subtype_Mark => New_Occurrence_Of (RTE (RE_Any), Loc));
            Set_Etype (Expr_Parameter, Typ);

            Any_Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier =>
                  Any,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Any), Loc));

            if Is_Derived_Type (Typ) and then not Is_Tagged_Type (Typ) then
               declare
                  Rt_Type : constant Entity_Id
                    := Root_Type (Typ);
                  Expr : constant Node_Id
                    := OK_Convert_To (
                         Rt_Type,
                         New_Occurrence_Of (Expr_Parameter, Loc));
               begin
                  Set_Expression (Any_Decl, Build_To_Any_Call (Expr, Decls));
               end;

            elsif Is_Record_Type (Typ) and then not Is_Tagged_Type (Typ) then
               if Nkind (Declaration_Node (Typ)) = N_Subtype_Declaration then
                  declare
                     Rt_Type : constant Entity_Id
                       := Etype (Typ);
                     Expr : constant Node_Id
                       := OK_Convert_To (
                            Rt_Type,
                            New_Occurrence_Of (Expr_Parameter, Loc));

                  begin
                     Set_Expression (Any_Decl,
                       Build_To_Any_Call (Expr, Decls));
                  end;

               else
                  declare
                     Disc : Entity_Id := Empty;
                     Rdef : constant Node_Id :=
                              Type_Definition (Declaration_Node (Typ));
                     Counter : Int := 0;
                     Elements : constant List_Id := New_List;

                     procedure TA_Rec_Add_Process_Element
                       (Stmts     : List_Id;
                        Container : Node_Or_Entity_Id;
                        Counter   : in out Int;
                        Rec       : Entity_Id;
                        Field     : Node_Id);

                     procedure TA_Append_Record_Traversal is
                        new Append_Record_Traversal
                          (Rec                 => Expr_Parameter,
                           Add_Process_Element => TA_Rec_Add_Process_Element);

                     --------------------------------
                     -- TA_Rec_Add_Process_Element --
                     --------------------------------

                     procedure TA_Rec_Add_Process_Element
                       (Stmts     : List_Id;
                        Container : Node_Or_Entity_Id;
                        Counter   : in out Int;
                        Rec       : Entity_Id;
                        Field     : Node_Id)
                     is
                        Field_Ref : Node_Id;

                     begin
                        if Nkind (Field) = N_Defining_Identifier then

                           --  A regular component

                           Field_Ref := Make_Selected_Component (Loc,
                             Prefix        => New_Occurrence_Of (Rec, Loc),
                             Selector_Name => New_Occurrence_Of (Field, Loc));
                           Set_Etype (Field_Ref, Etype (Field));

                           Append_To (Stmts,
                             Make_Procedure_Call_Statement (Loc,
                               Name =>
                                 New_Occurrence_Of (
                                   RTE (RE_Add_Aggregate_Element), Loc),
                               Parameter_Associations => New_List (
                                 New_Occurrence_Of (Any, Loc),
                                 Build_To_Any_Call (Field_Ref, Decls))));

                        else
                           --  A variant part

                           declare
                              Variant : Node_Id;
                              Struct_Counter : Int := 0;

                              Block_Decls : constant List_Id := New_List;
                              Block_Stmts : constant List_Id := New_List;
                              VP_Stmts    : List_Id;

                              Alt_List : constant List_Id := New_List;
                              Choice_List : List_Id;

                              Union_Any : constant Entity_Id :=
                                            Make_Defining_Identifier (Loc,
                                              New_Internal_Name ('U'));

                              Struct_Any : constant Entity_Id :=
                                             Make_Defining_Identifier (Loc,
                                                New_Internal_Name ('S'));

                              function Make_Discriminant_Reference
                                return Node_Id;
                              --  Build a selected component for the
                              --  discriminant of this variant part.

                              ---------------------------------
                              -- Make_Discriminant_Reference --
                              ---------------------------------

                              function Make_Discriminant_Reference
                                return Node_Id
                              is
                                 Nod : constant Node_Id :=
                                         Make_Selected_Component (Loc,
                                           Prefix        => Rec,
                                           Selector_Name =>
                                             Chars (Name (Field)));
                              begin
                                 Set_Etype (Nod, Name (Field));
                                 return Nod;
                              end Make_Discriminant_Reference;

                           begin
                              Append_To (Stmts,
                                Make_Block_Statement (Loc,
                                  Declarations =>
                                    Block_Decls,
                                  Handled_Statement_Sequence =>
                                    Make_Handled_Sequence_Of_Statements (Loc,
                                      Statements => Block_Stmts)));

                              Append_To (Block_Decls,
                                Make_Object_Declaration (Loc,
                                  Defining_Identifier => Union_Any,
                                  Object_Definition   =>
                                    New_Occurrence_Of (RTE (RE_Any), Loc),
                                  Expression =>
                                    Make_Function_Call (Loc,
                                      Name => New_Occurrence_Of (
                                                RTE (RE_Create_Any), Loc),
                                      Parameter_Associations => New_List (
                                        Make_Function_Call (Loc,
                                          Name =>
                                            New_Occurrence_Of (
                                              RTE (RE_Any_Member_Type), Loc),
                                          Parameter_Associations => New_List (
                                            New_Occurrence_Of (Container, Loc),
                                            Make_Integer_Literal (Loc,
                                              Counter)))))));

                              Append_To (Block_Decls,
                                Make_Object_Declaration (Loc,
                                  Defining_Identifier => Struct_Any,
                                  Object_Definition   =>
                                    New_Occurrence_Of (RTE (RE_Any), Loc),
                                  Expression =>
                                    Make_Function_Call (Loc,
                                      Name => New_Occurrence_Of (
                                        RTE (RE_Create_Any), Loc),
                                      Parameter_Associations => New_List (
                                        Make_Function_Call (Loc,
                                          Name =>
                                            New_Occurrence_Of (
                                              RTE (RE_Any_Member_Type), Loc),
                                          Parameter_Associations => New_List (
                                            New_Occurrence_Of (Union_Any, Loc),
                                            Make_Integer_Literal (Loc,
                                              Uint_0)))))));

                              Append_To (Block_Stmts,
                                Make_Case_Statement (Loc,
                                    Expression =>
                                      Make_Discriminant_Reference,
                                    Alternatives =>
                                      Alt_List));

                              Variant := First_Non_Pragma (Variants (Field));
                              while Present (Variant) loop
                                 Choice_List := New_Copy_List_Tree
                                   (Discrete_Choices (Variant));

                                 VP_Stmts := New_List;
                                 TA_Append_Record_Traversal (
                                   Stmts     => VP_Stmts,
                                   Clist     => Component_List (Variant),
                                   Container => Struct_Any,
                                   Counter   => Struct_Counter);

                                 --  Append discriminant value and inner struct
                                 --  to union aggregate.

                                 Append_To (VP_Stmts,
                                    Make_Procedure_Call_Statement (Loc,
                                      Name =>
                                        New_Occurrence_Of (
                                          RTE (RE_Add_Aggregate_Element), Loc),
                                      Parameter_Associations => New_List (
                                        New_Occurrence_Of (Union_Any, Loc),
                                          Build_To_Any_Call (
                                            Make_Discriminant_Reference,
                                            Block_Decls))));

                                 Append_To (VP_Stmts,
                                   Make_Procedure_Call_Statement (Loc,
                                     Name =>
                                       New_Occurrence_Of (
                                         RTE (RE_Add_Aggregate_Element), Loc),
                                     Parameter_Associations => New_List (
                                       New_Occurrence_Of (Union_Any, Loc),
                                       New_Occurrence_Of (Struct_Any, Loc))));

                                 --  Append union to outer aggregate

                                 Append_To (VP_Stmts,
                                   Make_Procedure_Call_Statement (Loc,
                                     Name =>
                                       New_Occurrence_Of (
                                         RTE (RE_Add_Aggregate_Element), Loc),
                                     Parameter_Associations => New_List (
                                       New_Occurrence_Of (Container, Loc),
                                       Make_Function_Call (Loc,
                                         Name => New_Occurrence_Of (
                                           RTE (RE_Any_Aggregate_Build), Loc),
                                         Parameter_Associations => New_List (
                                           New_Occurrence_Of (
                                             Union_Any, Loc))))));

                                 Append_To (Alt_List,
                                   Make_Case_Statement_Alternative (Loc,
                                     Discrete_Choices => Choice_List,
                                     Statements =>
                                       VP_Stmts));
                                 Next_Non_Pragma (Variant);
                              end loop;
                           end;
                        end if;
                     end TA_Rec_Add_Process_Element;

                  begin
                     --  First all discriminants

                     if Has_Discriminants (Typ) then
                        Disc := First_Discriminant (Typ);

                        while Present (Disc) loop
                           Append_To (Elements,
                             Make_Component_Association (Loc,
                               Choices => New_List (
                                 Make_Integer_Literal (Loc, Counter)),
                               Expression =>
                                 Build_To_Any_Call (
                                   Make_Selected_Component (Loc,
                                     Prefix        => Expr_Parameter,
                                     Selector_Name => Chars (Disc)),
                                   Decls)));
                           Counter := Counter + 1;
                           Next_Discriminant (Disc);
                        end loop;

                     else
                        --  Make elements an empty array

                        declare
                           Dummy_Any : constant Entity_Id :=
                                         Make_Defining_Identifier (Loc,
                                           Chars => New_Internal_Name ('A'));

                        begin
                           Append_To (Decls,
                             Make_Object_Declaration (Loc,
                               Defining_Identifier => Dummy_Any,
                               Object_Definition   =>
                                 New_Occurrence_Of (RTE (RE_Any), Loc)));

                           Append_To (Elements,
                             Make_Component_Association (Loc,
                               Choices => New_List (
                                 Make_Range (Loc,
                                   Low_Bound  =>
                                     Make_Integer_Literal (Loc, 1),
                                   High_Bound =>
                                     Make_Integer_Literal (Loc, 0))),
                               Expression =>
                                 New_Occurrence_Of (Dummy_Any, Loc)));
                        end;
                     end if;

                     Set_Expression (Any_Decl,
                       Make_Function_Call (Loc,
                         Name => New_Occurrence_Of (
                                   RTE (RE_Any_Aggregate_Build), Loc),
                         Parameter_Associations => New_List (
                           Result_TC,
                           Make_Aggregate (Loc,
                             Component_Associations => Elements))));
                     Result_TC := Empty;

                     --  ... then all components

                     TA_Append_Record_Traversal (Stms,
                       Clist     => Component_List (Rdef),
                       Container => Any,
                       Counter   => Counter);
                  end;
               end if;

            elsif Is_Array_Type (Typ) then
               declare
                  Constrained : constant Boolean := Is_Constrained (Typ);

                  procedure TA_Ary_Add_Process_Element
                    (Stmts   : List_Id;
                     Any     : Entity_Id;
                     Counter : Entity_Id;
                     Datum   : Node_Id);

                  --------------------------------
                  -- TA_Ary_Add_Process_Element --
                  --------------------------------

                  procedure TA_Ary_Add_Process_Element
                    (Stmts   : List_Id;
                     Any     : Entity_Id;
                     Counter : Entity_Id;
                     Datum   : Node_Id)
                  is
                     pragma Warnings (Off);
                     pragma Unreferenced (Counter);
                     pragma Warnings (On);

                     Element_Any : Node_Id;

                  begin
                     if Etype (Datum) = RTE (RE_Any) then
                        Element_Any := Datum;
                     else
                        Element_Any := Build_To_Any_Call (Datum, Decls);
                     end if;

                     Append_To (Stmts,
                       Make_Procedure_Call_Statement (Loc,
                         Name => New_Occurrence_Of (
                                   RTE (RE_Add_Aggregate_Element), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (Any, Loc),
                           Element_Any)));
                  end TA_Ary_Add_Process_Element;

                  procedure Append_To_Any_Array_Iterator is
                    new Append_Array_Traversal (
                      Subprogram => Fnam,
                      Arry       => Expr_Parameter,
                      Indices    => New_List,
                      Add_Process_Element => TA_Ary_Add_Process_Element);

                  Index : Node_Id;

               begin
                  Set_Expression (Any_Decl,
                    Make_Function_Call (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Create_Any), Loc),
                      Parameter_Associations => New_List (Result_TC)));
                  Result_TC := Empty;

                  if not Constrained then
                     Index := First_Index (Typ);
                     for J in 1 .. Number_Dimensions (Typ) loop
                        Append_To (Stms,
                          Make_Procedure_Call_Statement (Loc,
                            Name =>
                              New_Occurrence_Of (
                                RTE (RE_Add_Aggregate_Element), Loc),
                            Parameter_Associations => New_List (
                              New_Occurrence_Of (Any, Loc),
                              Build_To_Any_Call (
                                OK_Convert_To (Etype (Index),
                                  Make_Attribute_Reference (Loc,
                                    Prefix         =>
                                      New_Occurrence_Of (Expr_Parameter, Loc),
                                    Attribute_Name => Name_First,
                                    Expressions    => New_List (
                                      Make_Integer_Literal (Loc, J)))),
                                Decls))));
                        Next_Index (Index);
                     end loop;
                  end if;

                  Append_To_Any_Array_Iterator (Stms, Any);
               end;

            elsif Is_Integer_Type (Typ) or else Is_Unsigned_Type (Typ) then
               Set_Expression (Any_Decl,
                 Build_To_Any_Call (
                   OK_Convert_To (
                     Find_Numeric_Representation (Typ),
                     New_Occurrence_Of (Expr_Parameter, Loc)),
                   Decls));

            else
               --  Default: type is represented as an opaque sequence of bytes

               declare
                  Strm : constant Entity_Id := Make_Defining_Identifier (Loc,
                           New_Internal_Name ('S'));

               begin
                  --  Strm : aliased Buffer_Stream_Type;

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Strm,
                      Aliased_Present     =>
                        True,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Buffer_Stream_Type), Loc)));

                  --  Allocate_Buffer (Strm);

                  Append_To (Stms,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Allocate_Buffer), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Strm, Loc))));

                  --  T'Output (Strm'Access, E);

                  Append_To (Stms,
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Typ, Loc),
                        Attribute_Name => Name_Output,
                        Expressions => New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Occurrence_Of (Strm, Loc),
                            Attribute_Name => Name_Access),
                          New_Occurrence_Of (Expr_Parameter, Loc))));

                  --  BS_To_Any (Strm, A);

                  Append_To (Stms,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_BS_To_Any), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Strm, Loc),
                        New_Occurrence_Of (Any, Loc))));

                  --  Release_Buffer (Strm);

                  Append_To (Stms,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Release_Buffer), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Strm, Loc))));
               end;
            end if;

            Append_To (Decls, Any_Decl);

            if Present (Result_TC) then
               Append_To (Stms,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Occurrence_Of (RTE (RE_Set_TC), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Any, Loc),
                     Result_TC)));
            end if;

            Append_To (Stms,
              Make_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Any, Loc)));

            Decl :=
              Make_Subprogram_Body (Loc,
                Specification => Spec,
                Declarations => Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Stms));
         end Build_To_Any_Function;

         -------------------------
         -- Build_TypeCode_Call --
         -------------------------

         function Build_TypeCode_Call
           (Loc   : Source_Ptr;
            Typ   : Entity_Id;
            Decls : List_Id) return Node_Id
         is
            U_Type : Entity_Id  := Underlying_Type (Typ);
            --  The full view, if Typ is private; the completion,
            --  if Typ is incomplete.

            Fnam    : Entity_Id := Empty;
            Tnam    : Entity_Id := Empty;
            Pnam    : Entity_Id := Empty;
            Args    : List_Id := Empty_List;
            Lib_RE  : RE_Id := RE_Null;

            Expr : Node_Id;

         begin
            --  Special case System.PolyORB.Interface.Any: its primitives have
            --  not been set yet, so can't call Find_Inherited_TSS.

            if Typ = RTE (RE_Any) then
               Fnam := RTE (RE_TC_Any);

            else
               --  First simple case where the TypeCode is present
               --  in the type's TSS.

               Fnam := Find_Inherited_TSS (U_Type, TSS_TypeCode);

               if Present (Fnam) then

                  --  When a TypeCode TSS exists, it has a single parameter
                  --  that is an anonymous access to the corresponding type.
                  --  This parameter is not used in any way; its purpose is
                  --  solely to provide overloading of the TSS.

                  Tnam :=
                    Make_Defining_Identifier (Loc, New_Internal_Name ('T'));
                  Pnam :=
                    Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

                  Append_To (Decls,
                    Make_Full_Type_Declaration (Loc,
                      Defining_Identifier => Tnam,
                      Type_Definition =>
                        Make_Access_To_Object_Definition (Loc,
                          Subtype_Indication =>
                            New_Occurrence_Of (U_Type, Loc))));
                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Pnam,
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (Tnam, Loc),

                     --  Use a variable here to force proper freezing of Tnam

                      Expression          => Make_Null (Loc)));

                  --  Normally, calling _TypeCode with a null access parameter
                  --  should raise Constraint_Error, but this check is
                  --  suppressed for expanded code, and we do not care anyway
                  --  because we do not actually ever use this value.

                  Args := New_List (New_Occurrence_Of (Pnam, Loc));
               end if;
            end if;

            if No (Fnam) then
               if Sloc (U_Type) <= Standard_Location then

                  --  Do not try to build alias typecodes for subtypes from
                  --  Standard.

                  U_Type := Base_Type (U_Type);
               end if;

               if Is_Itype (U_Type) then
                  return Build_TypeCode_Call
                    (Loc, Associated_Node_For_Itype (U_Type), Decls);
               end if;

               if U_Type = Standard_Boolean then
                  Lib_RE := RE_TC_B;

               elsif U_Type = Standard_Character then
                  Lib_RE := RE_TC_C;

               elsif U_Type = Standard_Wide_Character then
                  Lib_RE := RE_TC_WC;

               elsif U_Type = Standard_Wide_Wide_Character then
                  Lib_RE := RE_TC_WWC;

               --  Floating point types

               elsif U_Type = Standard_Short_Float then
                  Lib_RE := RE_TC_SF;

               elsif U_Type = Standard_Float then
                  Lib_RE := RE_TC_F;

               elsif U_Type = Standard_Long_Float then
                  Lib_RE := RE_TC_LF;

               elsif U_Type = Standard_Long_Long_Float then
                  Lib_RE := RE_TC_LLF;

               --  Integer types (walk back to the base type)

               elsif U_Type = Etype (Standard_Short_Short_Integer) then
                     Lib_RE := RE_TC_SSI;

               elsif U_Type = Etype (Standard_Short_Integer) then
                  Lib_RE := RE_TC_SI;

               elsif U_Type = Etype (Standard_Integer) then
                  Lib_RE := RE_TC_I;

               elsif U_Type = Etype (Standard_Long_Integer) then
                  Lib_RE := RE_TC_LI;

               elsif U_Type = Etype (Standard_Long_Long_Integer) then
                  Lib_RE := RE_TC_LLI;

               --  Unsigned integer types

               elsif U_Type = RTE (RE_Short_Short_Unsigned) then
                  Lib_RE := RE_TC_SSU;

               elsif U_Type = RTE (RE_Short_Unsigned) then
                  Lib_RE := RE_TC_SU;

               elsif U_Type = RTE (RE_Unsigned) then
                  Lib_RE := RE_TC_U;

               elsif U_Type = RTE (RE_Long_Unsigned) then
                  Lib_RE := RE_TC_LU;

               elsif U_Type = RTE (RE_Long_Long_Unsigned) then
                  Lib_RE := RE_TC_LLU;

               elsif U_Type = Standard_String then
                  Lib_RE := RE_TC_String;

               --  Other (non-primitive) types

               else
                  declare
                     Decl : Entity_Id;
                  begin
                     Build_TypeCode_Function (Loc, U_Type, Decl, Fnam);
                     Append_To (Decls, Decl);
                  end;
               end if;

               if Lib_RE /= RE_Null then
                  Fnam := RTE (Lib_RE);
               end if;
            end if;

            --  Call the function

            Expr :=
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (Fnam, Loc),
                Parameter_Associations => Args);

            --  Allow Expr to be used as arg to Build_To_Any_Call immediately

            Set_Etype (Expr, RTE (RE_TypeCode));

            return Expr;
         end Build_TypeCode_Call;

         -----------------------------
         -- Build_TypeCode_Function --
         -----------------------------

         procedure Build_TypeCode_Function
           (Loc  : Source_Ptr;
            Typ  : Entity_Id;
            Decl : out Node_Id;
            Fnam : out Entity_Id)
         is
            Spec  : Node_Id;
            Decls : constant List_Id := New_List;
            Stms  : constant List_Id := New_List;

            TCNam : constant Entity_Id :=
                      Make_Stream_Procedure_Function_Name (Loc,
                        Typ, Name_uTypeCode);

            Parameters : List_Id;

            procedure Add_String_Parameter
              (S              : String_Id;
               Parameter_List : List_Id);
            --  Add a literal for S to Parameters

            procedure Add_TypeCode_Parameter
              (TC_Node        : Node_Id;
               Parameter_List : List_Id);
            --  Add the typecode for Typ to Parameters

            procedure Add_Long_Parameter
              (Expr_Node      : Node_Id;
               Parameter_List : List_Id);
            --  Add a signed long integer expression to Parameters

            procedure Initialize_Parameter_List
              (Name_String    : String_Id;
               Repo_Id_String : String_Id;
               Parameter_List : out List_Id);
            --  Return a list that contains the first two parameters
            --  for a parameterized typecode: name and repository id.

            function Make_Constructed_TypeCode
              (Kind       : Entity_Id;
               Parameters : List_Id) return Node_Id;
            --  Call TC_Build with the given kind and parameters

            procedure Return_Constructed_TypeCode (Kind : Entity_Id);
            --  Make a return statement that calls TC_Build with the given
            --  typecode kind, and the constructed parameters list.

            procedure Return_Alias_TypeCode (Base_TypeCode  : Node_Id);
            --  Return a typecode that is a TC_Alias for the given typecode

            --------------------------
            -- Add_String_Parameter --
            --------------------------

            procedure Add_String_Parameter
              (S              : String_Id;
               Parameter_List : List_Id)
            is
            begin
               Append_To (Parameter_List,
                 Make_Function_Call (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_TA_String), Loc),
                   Parameter_Associations => New_List (
                     Make_String_Literal (Loc, S))));
            end Add_String_Parameter;

            ----------------------------
            -- Add_TypeCode_Parameter --
            ----------------------------

            procedure Add_TypeCode_Parameter
              (TC_Node        : Node_Id;
               Parameter_List : List_Id)
            is
            begin
               Append_To (Parameter_List,
                 Make_Function_Call (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_TA_TC), Loc),
                   Parameter_Associations => New_List (
                     TC_Node)));
            end Add_TypeCode_Parameter;

            ------------------------
            -- Add_Long_Parameter --
            ------------------------

            procedure Add_Long_Parameter
              (Expr_Node      : Node_Id;
               Parameter_List : List_Id)
            is
            begin
               Append_To (Parameter_List,
                 Make_Function_Call (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_TA_LI), Loc),
                   Parameter_Associations => New_List (Expr_Node)));
            end Add_Long_Parameter;

            -------------------------------
            -- Initialize_Parameter_List --
            -------------------------------

            procedure Initialize_Parameter_List
              (Name_String    : String_Id;
               Repo_Id_String : String_Id;
               Parameter_List : out List_Id)
            is
            begin
               Parameter_List := New_List;
               Add_String_Parameter (Name_String, Parameter_List);
               Add_String_Parameter (Repo_Id_String, Parameter_List);
            end Initialize_Parameter_List;

            ---------------------------
            -- Return_Alias_TypeCode --
            ---------------------------

            procedure Return_Alias_TypeCode
              (Base_TypeCode  : Node_Id)
            is
            begin
               Add_TypeCode_Parameter (Base_TypeCode, Parameters);
               Return_Constructed_TypeCode (RTE (RE_TC_Alias));
            end Return_Alias_TypeCode;

            -------------------------------
            -- Make_Constructed_TypeCode --
            -------------------------------

            function Make_Constructed_TypeCode
              (Kind       : Entity_Id;
               Parameters : List_Id) return Node_Id
            is
               Constructed_TC : constant Node_Id :=
                 Make_Function_Call (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_TC_Build), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Kind, Loc),
                     Make_Aggregate (Loc,
                        Expressions => Parameters)));
            begin
               Set_Etype (Constructed_TC, RTE (RE_TypeCode));
               return Constructed_TC;
            end Make_Constructed_TypeCode;

            ---------------------------------
            -- Return_Constructed_TypeCode --
            ---------------------------------

            procedure Return_Constructed_TypeCode (Kind : Entity_Id) is
            begin
               Append_To (Stms,
                 Make_Return_Statement (Loc,
                   Expression =>
                      Make_Constructed_TypeCode (Kind, Parameters)));
            end Return_Constructed_TypeCode;

            ------------------
            -- Record types --
            ------------------

            procedure TC_Rec_Add_Process_Element
              (Params  : List_Id;
               Any     : Entity_Id;
               Counter : in out Int;
               Rec     : Entity_Id;
               Field   : Node_Id);

            procedure TC_Append_Record_Traversal is
              new Append_Record_Traversal (
                Rec                 => Empty,
                Add_Process_Element => TC_Rec_Add_Process_Element);

            --------------------------------
            -- TC_Rec_Add_Process_Element --
            --------------------------------

            procedure TC_Rec_Add_Process_Element
              (Params  : List_Id;
               Any     : Entity_Id;
               Counter : in out Int;
               Rec     : Entity_Id;
               Field   : Node_Id)
            is
               pragma Warnings (Off);
               pragma Unreferenced (Any, Counter, Rec);
               pragma Warnings (On);

            begin
               if Nkind (Field) = N_Defining_Identifier then

                  --  A regular component

                  Add_TypeCode_Parameter (
                    Build_TypeCode_Call (Loc, Etype (Field), Decls), Params);
                  Get_Name_String (Chars (Field));
                  Add_String_Parameter (String_From_Name_Buffer, Params);

               else

                  --  A variant part

                  declare
                     Discriminant_Type : constant Entity_Id :=
                                           Etype (Name (Field));

                     Is_Enum : constant Boolean :=
                                 Is_Enumeration_Type (Discriminant_Type);

                     Union_TC_Params : List_Id;

                     U_Name : constant Name_Id :=
                                New_External_Name (Chars (Typ), 'U', -1);

                     Name_Str         : String_Id;
                     Struct_TC_Params : List_Id;

                     Variant : Node_Id;
                     Choice  : Node_Id;
                     Default : constant Node_Id :=
                                 Make_Integer_Literal (Loc, -1);

                     Dummy_Counter : Int := 0;

                     procedure Add_Params_For_Variant_Components;
                     --  Add a struct TypeCode and a corresponding member name
                     --  to the union parameter list.

                     --  Ordering of declarations is a complete mess in this
                     --  area, it is supposed to be types/varibles, then
                     --  subprogram specs, then subprogram bodies ???

                     ---------------------------------------
                     -- Add_Params_For_Variant_Components --
                     ---------------------------------------

                     procedure Add_Params_For_Variant_Components
                     is
                        S_Name : constant Name_Id :=
                                   New_External_Name (U_Name, 'S', -1);

                     begin
                        Get_Name_String (S_Name);
                        Name_Str := String_From_Name_Buffer;
                        Initialize_Parameter_List
                          (Name_Str, Name_Str, Struct_TC_Params);

                        --  Build struct parameters

                        TC_Append_Record_Traversal (Struct_TC_Params,
                          Component_List (Variant),
                          Empty,
                          Dummy_Counter);

                        Add_TypeCode_Parameter
                          (Make_Constructed_TypeCode
                           (RTE (RE_TC_Struct), Struct_TC_Params),
                           Union_TC_Params);

                        Add_String_Parameter (Name_Str, Union_TC_Params);
                     end Add_Params_For_Variant_Components;

                  begin
                     Get_Name_String (U_Name);
                     Name_Str := String_From_Name_Buffer;

                     Initialize_Parameter_List
                       (Name_Str, Name_Str, Union_TC_Params);

                     Add_String_Parameter (Name_Str, Params);

                     --  Add union in enclosing parameter list

                     Add_TypeCode_Parameter
                       (Make_Constructed_TypeCode
                        (RTE (RE_TC_Union), Union_TC_Params),
                        Parameters);

                     --  Build union parameters

                     Add_TypeCode_Parameter
                       (Discriminant_Type, Union_TC_Params);
                     Add_Long_Parameter (Default, Union_TC_Params);

                     Variant := First_Non_Pragma (Variants (Field));
                     while Present (Variant) loop
                        Choice := First (Discrete_Choices (Variant));
                        while Present (Choice) loop
                           case Nkind (Choice) is
                              when N_Range =>
                                 declare
                                    L : constant Uint :=
                                          Expr_Value (Low_Bound (Choice));
                                    H : constant Uint :=
                                          Expr_Value (High_Bound (Choice));
                                    J : Uint := L;
                                    --  3.8.1(8) guarantees that the bounds of
                                    --  this range are static.

                                    Expr : Node_Id;

                                 begin
                                    while J <= H loop
                                       if Is_Enum then
                                          Expr := New_Occurrence_Of (
                                            Get_Enum_Lit_From_Pos (
                                              Discriminant_Type, J, Loc), Loc);
                                       else
                                          Expr :=
                                            Make_Integer_Literal (Loc, J);
                                       end if;
                                       Append_To (Union_TC_Params,
                                         Build_To_Any_Call (Expr, Decls));
                                       Add_Params_For_Variant_Components;
                                       J := J + Uint_1;
                                    end loop;
                                 end;

                              when N_Others_Choice =>
                                 Add_Long_Parameter (
                                   Make_Integer_Literal (Loc, 0),
                                   Union_TC_Params);
                                 Add_Params_For_Variant_Components;

                              when others =>
                                 Append_To (Union_TC_Params,
                                   Build_To_Any_Call (Choice, Decls));
                                 Add_Params_For_Variant_Components;

                           end case;

                        end loop;

                        Next_Non_Pragma (Variant);
                     end loop;

                  end;
               end if;
            end TC_Rec_Add_Process_Element;

            Type_Name_Str    : String_Id;
            Type_Repo_Id_Str : String_Id;

         begin
            pragma Assert (not Is_Itype (Typ));
            Fnam := TCNam;

            Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name => Fnam,
                Parameter_Specifications => Empty_List,
                Subtype_Mark => New_Occurrence_Of (RTE (RE_TypeCode), Loc));

            Build_Name_And_Repository_Id (Typ,
              Name_Str => Type_Name_Str, Repo_Id_Str => Type_Repo_Id_Str);
            Initialize_Parameter_List
              (Type_Name_Str, Type_Repo_Id_Str, Parameters);

            if Is_Derived_Type (Typ)
              and then not Is_Tagged_Type (Typ)
            then
               declare
                  D_Node : constant Node_Id := Declaration_Node (Typ);
                  Parent_Type : Entity_Id := Etype (Typ);
               begin

                  if Is_Enumeration_Type (Typ)
                    and then Nkind (D_Node) = N_Subtype_Declaration
                    and then Nkind (Original_Node (D_Node))
                    /= N_Subtype_Declaration
                  then

                     --  Parent_Type is the implicit intermediate base type
                     --  created by Build_Derived_Enumeration_Type.

                     Parent_Type := Etype (Parent_Type);
                  end if;

                  Return_Alias_TypeCode (
                    Build_TypeCode_Call (Loc, Parent_Type, Decls));
               end;

            elsif Is_Integer_Type (Typ)
              or else Is_Unsigned_Type (Typ)
            then
               Return_Alias_TypeCode (
                 Build_TypeCode_Call (Loc,
                   Find_Numeric_Representation (Typ), Decls));

            elsif Is_Record_Type (Typ)
              and then not Is_Tagged_Type (Typ)
            then
               if Nkind (Declaration_Node (Typ)) = N_Subtype_Declaration then
                  Return_Alias_TypeCode (
                    Build_TypeCode_Call (Loc, Etype (Typ), Decls));
               else
                  declare
                     Disc : Entity_Id := Empty;
                     Rdef : constant Node_Id :=
                       Type_Definition (Declaration_Node (Typ));
                     Dummy_Counter : Int := 0;
                  begin
                     --  First all discriminants

                     if Has_Discriminants (Typ) then
                        Disc := First_Discriminant (Typ);
                     end if;
                     while Present (Disc) loop
                        Add_TypeCode_Parameter (
                          Build_TypeCode_Call (Loc, Etype (Disc), Decls),
                          Parameters);
                        Get_Name_String (Chars (Disc));
                        Add_String_Parameter (
                          String_From_Name_Buffer,
                          Parameters);
                        Next_Discriminant (Disc);
                     end loop;

                     --  ... then all components

                     TC_Append_Record_Traversal
                       (Parameters, Component_List (Rdef),
                        Empty, Dummy_Counter);
                     Return_Constructed_TypeCode (RTE (RE_TC_Struct));
                  end;
               end if;

            elsif Is_Array_Type (Typ) then
               declare
                  Ndim           : constant Pos := Number_Dimensions (Typ);
                  Inner_TypeCode : Node_Id;
                  Constrained    : constant Boolean := Is_Constrained (Typ);
                  Indx           : Node_Id          := First_Index (Typ);

               begin
                  Inner_TypeCode := Build_TypeCode_Call (Loc,
                    Component_Type (Typ),
                    Decls);

                  for J in 1 .. Ndim loop
                     if Constrained then
                        Inner_TypeCode := Make_Constructed_TypeCode
                          (RTE (RE_TC_Array), New_List (
                            Build_To_Any_Call (
                              OK_Convert_To (RTE (RE_Long_Unsigned),
                                Make_Attribute_Reference (Loc,
                                  Prefix =>
                                    New_Occurrence_Of (Typ, Loc),
                                  Attribute_Name =>
                                    Name_Length,
                                  Expressions => New_List (
                                    Make_Integer_Literal (Loc,
                                      Ndim - J + 1)))),
                              Decls),
                            Build_To_Any_Call (Inner_TypeCode, Decls)));

                     else
                        --  Unconstrained case: add low bound for each
                        --  dimension.

                        Add_TypeCode_Parameter
                          (Build_TypeCode_Call (Loc, Etype (Indx), Decls),
                           Parameters);
                        Get_Name_String (New_External_Name ('L', J));
                        Add_String_Parameter (
                          String_From_Name_Buffer,
                          Parameters);
                        Next_Index (Indx);

                        Inner_TypeCode := Make_Constructed_TypeCode
                          (RTE (RE_TC_Sequence), New_List (
                            Build_To_Any_Call (
                              OK_Convert_To (RTE (RE_Long_Unsigned),
                                Make_Integer_Literal (Loc, 0)),
                              Decls),
                            Build_To_Any_Call (Inner_TypeCode, Decls)));
                     end if;
                  end loop;

                  if Constrained then
                     Return_Alias_TypeCode (Inner_TypeCode);
                  else
                     Add_TypeCode_Parameter (Inner_TypeCode, Parameters);
                     Start_String;
                     Store_String_Char ('V');
                     Add_String_Parameter (End_String, Parameters);
                     Return_Constructed_TypeCode (RTE (RE_TC_Struct));
                  end if;
               end;

            else
               --  Default: type is represented as an opaque sequence of bytes

               Return_Alias_TypeCode
                 (New_Occurrence_Of (RTE (RE_TC_Opaque), Loc));
            end if;

            Decl :=
              Make_Subprogram_Body (Loc,
                Specification => Spec,
                Declarations => Decls,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => Stms));
         end Build_TypeCode_Function;

         ---------------------------------
         -- Find_Numeric_Representation --
         ---------------------------------

         function Find_Numeric_Representation (Typ : Entity_Id)
           return Entity_Id
         is
            FST    : constant Entity_Id := First_Subtype (Typ);
            P_Size : constant Uint      := Esize (FST);

         begin
            if Is_Unsigned_Type (Typ) then
               if P_Size <= Standard_Short_Short_Integer_Size then
                  return RTE (RE_Short_Short_Unsigned);

               elsif P_Size <= Standard_Short_Integer_Size then
                  return RTE (RE_Short_Unsigned);

               elsif P_Size <= Standard_Integer_Size then
                  return RTE (RE_Unsigned);

               elsif P_Size <= Standard_Long_Integer_Size then
                  return RTE (RE_Long_Unsigned);

               else
                  return RTE (RE_Long_Long_Unsigned);
               end if;

            elsif Is_Integer_Type (Typ) then
               if P_Size <= Standard_Short_Short_Integer_Size then
                  return Standard_Short_Short_Integer;

               elsif P_Size <= Standard_Short_Integer_Size then
                  return Standard_Short_Integer;

               elsif P_Size <= Standard_Integer_Size then
                  return Standard_Integer;

               elsif P_Size <= Standard_Long_Integer_Size then
                  return Standard_Long_Integer;

               else
                  return Standard_Long_Long_Integer;
               end if;

            elsif Is_Floating_Point_Type (Typ) then
               if P_Size <= Standard_Short_Float_Size then
                  return Standard_Short_Float;

               elsif P_Size <= Standard_Float_Size then
                  return Standard_Float;

               elsif P_Size <= Standard_Long_Float_Size then
                  return Standard_Long_Float;

               else
                  return Standard_Long_Long_Float;
               end if;

            else
               raise Program_Error;
            end if;

            --  TBD: fixed point types???
            --  TBverified numeric types with a biased representation???

         end Find_Numeric_Representation;

         ---------------------------
         -- Append_Array_Traversal --
         ---------------------------

         procedure Append_Array_Traversal
           (Stmts   : List_Id;
            Any     : Entity_Id;
            Counter : Entity_Id := Empty;
            Depth   : Pos       := 1)
         is
            Loc         : constant Source_Ptr := Sloc (Subprogram);
            Typ         : constant Entity_Id  := Etype (Arry);
            Constrained : constant Boolean    := Is_Constrained (Typ);
            Ndim        : constant Pos        := Number_Dimensions (Typ);

            Inner_Any, Inner_Counter : Entity_Id;

            Loop_Stm    : Node_Id;
            Inner_Stmts : constant List_Id := New_List;

         begin
            if Depth > Ndim then

               --  Processing for one element of an array

               declare
                  Element_Expr : constant Node_Id :=
                                   Make_Indexed_Component (Loc,
                                     New_Occurrence_Of (Arry, Loc),
                                     Indices);

               begin
                  Set_Etype (Element_Expr, Component_Type (Typ));
                  Add_Process_Element (Stmts,
                    Any     => Any,
                    Counter => Counter,
                    Datum   => Element_Expr);
               end;

               return;
            end if;

            Append_To (Indices,
              Make_Identifier (Loc, New_External_Name ('L', Depth)));

            if Constrained then
               Inner_Any := Any;
               Inner_Counter := Counter;
            else
               Inner_Any := Make_Defining_Identifier (Loc,
                 New_External_Name ('A', Depth));
               Set_Etype (Inner_Any, RTE (RE_Any));

               if Present (Counter) then
                  Inner_Counter := Make_Defining_Identifier (Loc,
                    New_External_Name ('J', Depth));
               else
                  Inner_Counter := Empty;
               end if;
            end if;

            Append_Array_Traversal (Inner_Stmts,
              Any     => Inner_Any,
              Counter => Inner_Counter,
              Depth   => Depth + 1);

            Loop_Stm :=
              Make_Implicit_Loop_Statement (Subprogram,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier =>
                          Make_Defining_Identifier (Loc,
                            Chars => New_External_Name ('L', Depth)),

                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Arry, Loc),
                            Attribute_Name => Name_Range,

                            Expressions => New_List (
                              Make_Integer_Literal (Loc, Depth))))),
                Statements => Inner_Stmts);

            if Constrained then
               Append_To (Stmts, Loop_Stm);
               return;
            end if;

            declare
               Decls       : constant List_Id := New_List;
               Dimen_Stmts : constant List_Id := New_List;
               Length_Node : Node_Id;

               Inner_Any_TypeCode : constant Entity_Id :=
                                      Make_Defining_Identifier (Loc,
                                        New_External_Name ('T', Depth));

               Inner_Any_TypeCode_Expr : Node_Id;

            begin
               if Depth = 1 then
                  Inner_Any_TypeCode_Expr :=
                    Make_Function_Call (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Any_Member_Type), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (Any, Loc),
                        Make_Integer_Literal (Loc, Ndim)));
               else
                  Inner_Any_TypeCode_Expr :=
                    Make_Function_Call (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_Content_Type), Loc),
                      Parameter_Associations => New_List (
                        Make_Identifier (Loc,
                          New_External_Name ('T', Depth - 1))));
               end if;

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Inner_Any_TypeCode,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (
                                            RTE (RE_TypeCode), Loc),
                   Expression          => Inner_Any_TypeCode_Expr));
               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Inner_Any,
                   Object_Definition   =>
                     New_Occurrence_Of (RTE (RE_Any), Loc),
                   Expression          =>
                     Make_Function_Call (Loc,
                       Name =>
                         New_Occurrence_Of (
                           RTE (RE_Create_Any), Loc),
                       Parameter_Associations => New_List (
                         New_Occurrence_Of (Inner_Any_TypeCode, Loc)))));

               if Present (Inner_Counter) then
                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Inner_Counter,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Long_Unsigned), Loc),
                      Expression          =>
                        Make_Integer_Literal (Loc, 0)));
               end if;

               Length_Node := Make_Attribute_Reference (Loc,
                     Prefix         => New_Occurrence_Of (Arry, Loc),
                     Attribute_Name => Name_Length,
                     Expressions    =>
                       New_List (Make_Integer_Literal (Loc, Depth)));
               Set_Etype (Length_Node, RTE (RE_Long_Unsigned));

               Add_Process_Element (Dimen_Stmts,
                 Datum   => Length_Node,
                 Any     => Inner_Any,
                 Counter => Inner_Counter);

               --  Loop_Stm does approrpriate processing for each element
               --  of Inner_Any.

               Append_To (Dimen_Stmts, Loop_Stm);

               --  Link outer and inner any

               Add_Process_Element (Dimen_Stmts,
                 Any     => Any,
                 Counter => Counter,
                 Datum   => New_Occurrence_Of (Inner_Any, Loc));

               Append_To (Stmts,
                 Make_Block_Statement (Loc,
                   Declarations =>
                     Decls,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => Dimen_Stmts)));
            end;
         end Append_Array_Traversal;

         -----------------------------------------
         -- Make_Stream_Procedure_Function_Name --
         -----------------------------------------

         function Make_Stream_Procedure_Function_Name
           (Loc : Source_Ptr;
            Typ : Entity_Id;
            Nam : Name_Id) return Entity_Id
         is
         begin
            --  For tagged types, we use a canonical name so that it matches
            --  the primitive spec. For all other cases, we use a serialized
            --  name so that multiple generations of the same procedure do not
            --  clash.

            if Is_Tagged_Type (Typ) then
               return Make_Defining_Identifier (Loc, Nam);
            else
               return Make_Defining_Identifier (Loc,
                   Chars =>
                     New_External_Name (Nam, ' ', Increment_Serial_Number));
            end if;
         end Make_Stream_Procedure_Function_Name;
      end Helpers;

      -----------------------------------
      -- Reserve_NamingContext_Methods --
      -----------------------------------

      procedure Reserve_NamingContext_Methods is
         Str_Resolve : constant String := "resolve";
      begin
         Name_Buffer (1 .. Str_Resolve'Length) := Str_Resolve;
         Name_Len := Str_Resolve'Length;
         Overload_Counter_Table.Set (Name_Find, 1);
      end Reserve_NamingContext_Methods;

   end PolyORB_Support;

   -------------------------------
   -- RACW_Type_Is_Asynchronous --
   -------------------------------

   procedure RACW_Type_Is_Asynchronous (RACW_Type : Entity_Id) is
      Asynchronous_Flag : constant Entity_Id :=
                            Asynchronous_Flags_Table.Get (RACW_Type);
   begin
      Replace (Expression (Parent (Asynchronous_Flag)),
        New_Occurrence_Of (Standard_True, Sloc (Asynchronous_Flag)));
   end RACW_Type_Is_Asynchronous;

   -------------------------
   -- RCI_Package_Locator --
   -------------------------

   function RCI_Package_Locator
     (Loc          : Source_Ptr;
      Package_Spec : Node_Id) return Node_Id
   is
      Inst     : Node_Id;
      Pkg_Name : String_Id;

   begin
      Get_Library_Unit_Name_String (Package_Spec);
      Pkg_Name := String_From_Name_Buffer;
      Inst :=
        Make_Package_Instantiation (Loc,
          Defining_Unit_Name   =>
            Make_Defining_Identifier (Loc, New_Internal_Name ('R')),
          Name                 =>
            New_Occurrence_Of (RTE (RE_RCI_Locator), Loc),
          Generic_Associations => New_List (
            Make_Generic_Association (Loc,
              Selector_Name                     =>
                Make_Identifier (Loc, Name_RCI_Name),
              Explicit_Generic_Actual_Parameter =>
                Make_String_Literal (Loc,
                  Strval => Pkg_Name))));

      RCI_Locator_Table.Set (Defining_Unit_Name (Package_Spec),
        Defining_Unit_Name (Inst));
      return Inst;
   end RCI_Package_Locator;

   -----------------------------------------------
   -- Remote_Types_Tagged_Full_View_Encountered --
   -----------------------------------------------

   procedure Remote_Types_Tagged_Full_View_Encountered
     (Full_View : Entity_Id)
   is
      Stub_Elements : constant Stub_Structure :=
                        Stubs_Table.Get (Full_View);
   begin
      if Stub_Elements /= Empty_Stub_Structure then
         Add_RACW_Primitive_Declarations_And_Bodies
           (Full_View,
            Stub_Elements.RPC_Receiver_Decl,
            List_Containing (Declaration_Node (Full_View)));
      end if;
   end Remote_Types_Tagged_Full_View_Encountered;

   -------------------
   -- Scope_Of_Spec --
   -------------------

   function Scope_Of_Spec (Spec : Node_Id) return Entity_Id is
      Unit_Name : Node_Id := Defining_Unit_Name (Spec);

   begin
      while Nkind (Unit_Name) /= N_Defining_Identifier loop
         Unit_Name := Defining_Identifier (Unit_Name);
      end loop;

      return Unit_Name;
   end Scope_Of_Spec;

   ----------------------
   -- Set_Renaming_TSS --
   ----------------------

   procedure Set_Renaming_TSS
     (Typ     : Entity_Id;
      Nam     : Entity_Id;
      TSS_Nam : TSS_Name_Type)
   is
      Loc  : constant Source_Ptr := Sloc (Nam);
      Spec : constant Node_Id := Parent (Nam);

      TSS_Node : constant Node_Id :=
                   Make_Subprogram_Renaming_Declaration (Loc,
                     Specification =>
                       Copy_Specification (Loc,
                         Spec     => Spec,
                         New_Name => Make_TSS_Name (Typ, TSS_Nam)),
                       Name => New_Occurrence_Of (Nam, Loc));

      Snam : constant Entity_Id :=
               Defining_Unit_Name (Specification (TSS_Node));

   begin
      if Nkind (Spec) = N_Function_Specification then
         Set_Ekind (Snam, E_Function);
         Set_Etype (Snam, Entity (Subtype_Mark (Spec)));
      else
         Set_Ekind (Snam, E_Procedure);
         Set_Etype (Snam, Standard_Void_Type);
      end if;

      Set_TSS (Typ, Snam);
   end Set_Renaming_TSS;

   ----------------------------------------------
   -- Specific_Add_Obj_RPC_Receiver_Completion --
   ----------------------------------------------

   procedure Specific_Add_Obj_RPC_Receiver_Completion
     (Loc           : Source_Ptr;
      Decls         : List_Id;
      RPC_Receiver  : Entity_Id;
      Stub_Elements : Stub_Structure) is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            PolyORB_Support.Add_Obj_RPC_Receiver_Completion (Loc,
              Decls, RPC_Receiver, Stub_Elements);
         when others =>
            GARLIC_Support.Add_Obj_RPC_Receiver_Completion (Loc,
              Decls, RPC_Receiver, Stub_Elements);
      end case;
   end Specific_Add_Obj_RPC_Receiver_Completion;

   --------------------------------
   -- Specific_Add_RACW_Features --
   --------------------------------

   procedure Specific_Add_RACW_Features
     (RACW_Type         : Entity_Id;
      Desig             : Entity_Id;
      Stub_Type         : Entity_Id;
      Stub_Type_Access  : Entity_Id;
      RPC_Receiver_Decl : Node_Id;
      Declarations      : List_Id) is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            PolyORB_Support.Add_RACW_Features (
              RACW_Type,
              Desig,
              Stub_Type,
              Stub_Type_Access,
              RPC_Receiver_Decl,
              Declarations);

         when others =>
            GARLIC_Support.Add_RACW_Features (
              RACW_Type,
              Stub_Type,
              Stub_Type_Access,
              RPC_Receiver_Decl,
              Declarations);
      end case;
   end Specific_Add_RACW_Features;

   --------------------------------
   -- Specific_Add_RAST_Features --
   --------------------------------

   procedure Specific_Add_RAST_Features
     (Vis_Decl : Node_Id;
      RAS_Type : Entity_Id) is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            PolyORB_Support.Add_RAST_Features (Vis_Decl, RAS_Type);
         when others =>
            GARLIC_Support.Add_RAST_Features (Vis_Decl, RAS_Type);
      end case;
   end Specific_Add_RAST_Features;

   --------------------------------------------------
   -- Specific_Add_Receiving_Stubs_To_Declarations --
   --------------------------------------------------

   procedure Specific_Add_Receiving_Stubs_To_Declarations
     (Pkg_Spec : Node_Id;
      Decls    : List_Id)
   is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            PolyORB_Support.Add_Receiving_Stubs_To_Declarations (
              Pkg_Spec, Decls);
         when others =>
            GARLIC_Support.Add_Receiving_Stubs_To_Declarations (
              Pkg_Spec, Decls);
      end case;
   end Specific_Add_Receiving_Stubs_To_Declarations;

   ------------------------------------------
   -- Specific_Build_General_Calling_Stubs --
   ------------------------------------------

   procedure Specific_Build_General_Calling_Stubs
     (Decls                     : List_Id;
      Statements                : List_Id;
      Target                    : RPC_Target;
      Subprogram_Id             : Node_Id;
      Asynchronous              : Node_Id   := Empty;
      Is_Known_Asynchronous     : Boolean   := False;
      Is_Known_Non_Asynchronous : Boolean   := False;
      Is_Function               : Boolean;
      Spec                      : Node_Id;
      Stub_Type                 : Entity_Id := Empty;
      RACW_Type                 : Entity_Id := Empty;
      Nod                       : Node_Id)
   is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            PolyORB_Support.Build_General_Calling_Stubs (
              Decls,
              Statements,
              Target.Object,
              Subprogram_Id,
              Asynchronous,
              Is_Known_Asynchronous,
              Is_Known_Non_Asynchronous,
              Is_Function,
              Spec,
              Stub_Type,
              RACW_Type,
              Nod);
         when others =>
            GARLIC_Support.Build_General_Calling_Stubs (
              Decls,
              Statements,
              Target.Partition,
              Target.RPC_Receiver,
              Subprogram_Id,
              Asynchronous,
              Is_Known_Asynchronous,
              Is_Known_Non_Asynchronous,
              Is_Function,
              Spec,
              Stub_Type,
              RACW_Type,
              Nod);
      end case;
   end Specific_Build_General_Calling_Stubs;

   --------------------------------------
   -- Specific_Build_RPC_Receiver_Body --
   --------------------------------------

   procedure Specific_Build_RPC_Receiver_Body
     (RPC_Receiver : Entity_Id;
      Request      : out Entity_Id;
      Subp_Id      : out Entity_Id;
      Subp_Index   : out Entity_Id;
      Stmts        : out List_Id;
      Decl         : out Node_Id)
   is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            PolyORB_Support.Build_RPC_Receiver_Body
              (RPC_Receiver,
               Request,
               Subp_Id,
               Subp_Index,
               Stmts,
               Decl);
         when others =>
            GARLIC_Support.Build_RPC_Receiver_Body
              (RPC_Receiver,
               Request,
               Subp_Id,
               Subp_Index,
               Stmts,
               Decl);
      end case;
   end Specific_Build_RPC_Receiver_Body;

   --------------------------------
   -- Specific_Build_Stub_Target --
   --------------------------------

   function Specific_Build_Stub_Target
     (Loc                   : Source_Ptr;
      Decls                 : List_Id;
      RCI_Locator           : Entity_Id;
      Controlling_Parameter : Entity_Id) return RPC_Target is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            return PolyORB_Support.Build_Stub_Target (Loc,
                     Decls, RCI_Locator, Controlling_Parameter);
         when others =>
            return GARLIC_Support.Build_Stub_Target (Loc,
                     Decls, RCI_Locator, Controlling_Parameter);
      end case;
   end Specific_Build_Stub_Target;

   ------------------------------
   -- Specific_Build_Stub_Type --
   ------------------------------

   procedure Specific_Build_Stub_Type
     (RACW_Type         : Entity_Id;
      Stub_Type         : Entity_Id;
      Stub_Type_Decl    : out Node_Id;
      RPC_Receiver_Decl : out Node_Id)
   is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            PolyORB_Support.Build_Stub_Type (
              RACW_Type, Stub_Type,
              Stub_Type_Decl, RPC_Receiver_Decl);
         when others =>
            GARLIC_Support.Build_Stub_Type (
              RACW_Type, Stub_Type,
              Stub_Type_Decl, RPC_Receiver_Decl);
      end case;
   end Specific_Build_Stub_Type;

   function Specific_Build_Subprogram_Receiving_Stubs
     (Vis_Decl                 : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Parent_Primitive         : Entity_Id := Empty) return Node_Id is
   begin
      case Get_PCS_Name is
         when Name_PolyORB_DSA =>
            return PolyORB_Support.Build_Subprogram_Receiving_Stubs (
                     Vis_Decl,
                     Asynchronous,
                     Dynamically_Asynchronous,
                     Stub_Type,
                     RACW_Type,
                     Parent_Primitive);
         when others =>
            return GARLIC_Support.Build_Subprogram_Receiving_Stubs (
                     Vis_Decl,
                     Asynchronous,
                     Dynamically_Asynchronous,
                     Stub_Type,
                     RACW_Type,
                     Parent_Primitive);
      end case;
   end Specific_Build_Subprogram_Receiving_Stubs;

   --------------------------
   -- Underlying_RACW_Type --
   --------------------------

   function Underlying_RACW_Type (RAS_Typ : Entity_Id) return Entity_Id is
      Record_Type : Entity_Id;

   begin
      if Ekind (RAS_Typ) = E_Record_Type then
         Record_Type := RAS_Typ;
      else
         pragma Assert (Present (Equivalent_Type (RAS_Typ)));
         Record_Type := Equivalent_Type (RAS_Typ);
      end if;

      return
        Etype (Subtype_Indication (
          Component_Definition (
           First (Component_Items (Component_List (
            Type_Definition (Declaration_Node (Record_Type))))))));
   end Underlying_RACW_Type;

end Exp_Dist;
