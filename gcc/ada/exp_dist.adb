------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P_ D I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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
with Sem_Util;    use Sem_Util;
with Sinfo;       use Sinfo;
with Snames;      use Snames;
with Stand;       use Stand;
with Stringt;     use Stringt;
with Tbuild;      use Tbuild;
with Uintp;       use Uintp;
with Uname;       use Uname;

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
   --  RAS information lookup operation.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Add_RAS_Proxy_And_Analyze
     (Decls              :     List_Id;
      Vis_Decl           :     Node_Id;
      All_Calls_Remote_E :     Entity_Id;
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

   function Get_Subprogram_Id (E : Entity_Id) return Int;
   --  Given a subprogram defined in a RCI package, get its subprogram id
   --  which will be used for remote calls.

   function Build_Get_Unique_RP_Call
     (Loc       : Source_Ptr;
      Pointer   : Entity_Id;
      Stub_Type : Entity_Id) return List_Id;
   --  Build a call to Get_Unique_Remote_Pointer (Pointer),
   --  followed by a tag fixup (Get_Unique_Remote_Pointer may have
   --  changed Pointer'Tag to RACW_Stub_Type'Tag, while the desired
   --  tag is that of Stub_Type).

   procedure Build_General_Calling_Stubs
     (Decls                     : List_Id;
      Statements                : List_Id;
      Target_Partition          : Entity_Id;
      RPC_Receiver              : Node_Id;
      Subprogram_Id             : Node_Id;
      Asynchronous              : Node_Id := Empty;
      Is_Known_Asynchronous     : Boolean := False;
      Is_Known_Non_Asynchronous : Boolean := False;
      Is_Function               : Boolean;
      Spec                      : Node_Id;
      Stub_Type                 : Entity_Id := Empty;
      Nod                       : Node_Id);
   --  Build calling stubs for general purpose. The parameters are:
   --    Decls             : a place to put declarations
   --    Statements        : a place to put statements
   --    Target_Partition  : a node containing the target partition that must
   --                        be a N_Defining_Identifier
   --    RPC_Receiver      : a node containing the RPC receiver
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

   function Build_Subprogram_Calling_Stubs
     (Vis_Decl                 : Node_Id;
      Subp_Id                  : Int;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
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

   function Build_Subprogram_Receiving_Stubs
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

   function Build_RPC_Receiver_Specification
     (RPC_Receiver     : Entity_Id;
      Stream_Parameter : Entity_Id;
      Result_Parameter : Entity_Id) return Node_Id;
   --  Make a subprogram specification for an RPC receiver,
   --  with the given defining unit name and formal parameters.

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

   procedure Add_Receiving_Stubs_To_Declarations
     (Pkg_Spec : Node_Id;
      Decls    : List_Id);
   --  Add receiving stubs to the declarative part

   procedure Add_RAS_Dereference_TSS (N : Node_Id);
   --  Add a subprogram body for RAS Dereference TSS

   procedure Add_RAS_Access_TSS (N : Node_Id);
   --  Add a subprogram body for RAS Access TSS

   function Could_Be_Asynchronous (Spec : Node_Id) return Boolean;
   --  Return True if nothing prevents the program whose specification is
   --  given to be asynchronous (i.e. no out parameter).

   function Get_Pkg_Name_String_Id (Decl_Node : Node_Id) return String_Id;
   function Get_String_Id (Val : String) return String_Id;
   --  Ugly functions used to retrieve a package name. Inherited from the
   --  old exp_dist.adb and not rewritten yet ???

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

   function Scope_Of_Spec (Spec : Node_Id) return Entity_Id;
   --  Return the scope represented by a given spec

   function Need_Extra_Constrained (Parameter : Node_Id) return Boolean;
   --  Return True if the current parameter needs an extra formal to reflect
   --  its constrained status.

   function Is_RACW_Controlling_Formal
     (Parameter : Node_Id; Stub_Type : Entity_Id) return Boolean;
   --  Return True if the current parameter is a controlling formal argument
   --  of type Stub_Type or access to Stub_Type.

   type Stub_Structure is record
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Object_RPC_Receiver : Entity_Id;
      RPC_Receiver_Stream : Entity_Id;
      RPC_Receiver_Result : Entity_Id;
      RACW_Type           : Entity_Id;
   end record;
   --  This structure is necessary because of the two phases analysis of
   --  a RACW declaration occurring in the same Remote_Types package as the
   --  designated type. RACW_Type is any of the RACW types pointing on this
   --  designated type, it is used here to save an anonymous type creation
   --  for each primitive operation.

   Empty_Stub_Structure : constant Stub_Structure :=
     (Empty, Empty, Empty, Empty, Empty, Empty);

   type Hash_Index is range 0 .. 50;
   function Hash (F : Entity_Id) return Hash_Index;

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
   --  the generic instantiation of RCI_Info for this package.

   package RCI_Calling_Stubs_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Entity_Id,
                         No_Element => Empty,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RCI subprogram and the corresponding calling stubs

   procedure Add_Stub_Type
     (Designated_Type     : Entity_Id;
      RACW_Type           : Entity_Id;
      Decls               : List_Id;
      Stub_Type           : out Entity_Id;
      Stub_Type_Access    : out Entity_Id;
      Object_RPC_Receiver : out Entity_Id;
      Existing            : out Boolean);
   --  Add the declaration of the stub type, the access to stub type and the
   --  object RPC receiver at the end of Decls. If these already exist,
   --  then nothing is added in the tree but the right values are returned
   --  anyhow and Existing is set to True.

   procedure Add_RACW_Asynchronous_Flag
     (Declarations : List_Id;
      RACW_Type    : Entity_Id);
   --  Declare a boolean constant associated with RACW_Type whose value
   --  indicates at run time whether a pragma Asynchronous applies to it.

   procedure Add_RACW_Read_Attribute
     (RACW_Type           : Entity_Id;
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Declarations        : List_Id);
   --  Add Read attribute in Decls for the RACW type. The Read attribute
   --  is added right after the RACW_Type declaration while the body is
   --  inserted after Declarations.

   procedure Add_RACW_Write_Attribute
     (RACW_Type           : Entity_Id;
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Object_RPC_Receiver : Entity_Id;
      Declarations        : List_Id);
   --  Same thing for the Write attribute

   procedure Add_RACW_Read_Write_Attributes
     (RACW_Type           : Entity_Id;
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Object_RPC_Receiver : Entity_Id;
      Declarations        : List_Id);
   --  Add Read and Write attributes declarations and bodies for a given
   --  RACW type. The declarations are added just after the declaration
   --  of the RACW type itself, while the bodies are inserted at the end
   --  of Decls.

   function RCI_Package_Locator
     (Loc          : Source_Ptr;
      Package_Spec : Node_Id) return Node_Id;
   --  Instantiate the generic package RCI_Info in order to locate the
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
      Stream   : Entity_Id) return Node_Id;
   --  Return a function with the following form:
   --    function R return Var_Type is
   --    begin
   --       return Var_Type'Input (S);
   --    exception
   --       when E : Ada.Tags.Tag_Error =>
   --           Raise_Exception (Program_Error'Identity,
   --                            Exception_Message (E));
   --    end R;

   ------------------------------------
   -- Local variables and structures --
   ------------------------------------

   RCI_Cache : Node_Id;

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

   begin
      --  The first thing added is an instantiation of the generic package
      --  System.Partition_interface.RCI_Info with the name of the (current)
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

      Current_Declaration := First (Visible_Declarations (Pkg_Spec));
      while Current_Declaration /= Empty loop
         if Nkind (Current_Declaration) = N_Subprogram_Declaration
           and then Comes_From_Source (Current_Declaration)
         then
            pragma Assert (Current_Subprogram_Number =
              Get_Subprogram_Id (Defining_Unit_Name (Specification (
                Current_Declaration))));

            Subp_Stubs :=
              Build_Subprogram_Calling_Stubs (
                Vis_Decl     => Current_Declaration,
                Subp_Id      => Current_Subprogram_Number,
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

      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Object_RPC_Receiver : Entity_Id;
      Existing            : Boolean;

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
         Object_RPC_Receiver => Object_RPC_Receiver,
         Existing            => Existing);

      Add_RACW_Asynchronous_Flag
        (Declarations        => Decls,
         RACW_Type           => RACW_Type);

      Add_RACW_Read_Write_Attributes
        (RACW_Type           => RACW_Type,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Object_RPC_Receiver => Object_RPC_Receiver,
         Declarations        => Decls);

      if not Same_Scope and then not Existing then

         --  The RACW has been declared in another scope than the designated
         --  type and has not been handled by another RACW in the same package
         --  as the first one, so add primitive for the stub type here.

         Add_RACW_Primitive_Declarations_And_Bodies
           (Designated_Type  => Desig,
            Insertion_Node   =>
              Parent (Declaration_Node (Object_RPC_Receiver)),
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
      --  Set sloc of generated declaration copy of insertion node sloc, so
      --  the declarations are recognized as belonging to the current package.

      Loc : constant Source_Ptr := Sloc (Insertion_Node);

      Stub_Elements : constant Stub_Structure :=
                        Stubs_Table.Get (Designated_Type);

      pragma Assert (Stub_Elements /= Empty_Stub_Structure);

      Current_Insertion_Node : Node_Id := Insertion_Node;

      RPC_Receiver_Declarations      : List_Id;
      RPC_Receiver_Statements        : List_Id;
      RPC_Receiver_Case_Alternatives : constant List_Id := New_List;
      RPC_Receiver_Subp_Id           : Entity_Id;

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

      --  Build callers, receivers for every primitive operations and a RPC
      --  receiver for this type.

      if Present (Primitive_Operations (Designated_Type)) then

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

               Current_Primitive_Body :=
                 Build_Subprogram_Calling_Stubs
                   (Vis_Decl                 => Current_Primitive_Decl,
                    Subp_Id                  => Current_Primitive_Number,
                    Asynchronous             => Possibly_Asynchronous,
                    Dynamically_Asynchronous => Possibly_Asynchronous,
                    Stub_Type                => Stub_Elements.Stub_Type);
               Append_To (Decls, Current_Primitive_Body);

               --  Analyzing the body here would cause the Stub type to be
               --  frozen, thus preventing subsequent primitive declarations.
               --  For this reason, it will be analyzed later in the
               --  regular flow.

               --  Build the receiver stubs

               Current_Receiver_Body :=
                 Build_Subprogram_Receiving_Stubs
                   (Vis_Decl                 => Current_Primitive_Decl,
                    Asynchronous             => Possibly_Asynchronous,
                    Dynamically_Asynchronous => Possibly_Asynchronous,
                    Stub_Type                => Stub_Elements.Stub_Type,
                    RACW_Type                => Stub_Elements.RACW_Type,
                    Parent_Primitive         => Current_Primitive);

               Current_Receiver :=
                  Defining_Unit_Name (Specification (Current_Receiver_Body));

               Append_To (Decls, Current_Receiver_Body);

               --  Add a case alternative to the receiver

               Append_To (RPC_Receiver_Case_Alternatives,
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices => New_List (
                     Make_Integer_Literal (Loc, Current_Primitive_Number)),

                   Statements       => New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name                   =>
                         New_Occurrence_Of (Current_Receiver, Loc),
                       Parameter_Associations => New_List (
                         New_Occurrence_Of
                           (Stub_Elements.RPC_Receiver_Stream, Loc),
                         New_Occurrence_Of
                           (Stub_Elements.RPC_Receiver_Result, Loc))))));

               --  Increment the index of current primitive

               Current_Primitive_Number := Current_Primitive_Number + 1;
            end if;

            Next_Elmt (Current_Primitive_Elmt);
         end loop;
      end if;

      --  Build the case statement and the heart of the subprogram

      Append_To (RPC_Receiver_Case_Alternatives,
        Make_Case_Statement_Alternative (Loc,
          Discrete_Choices => New_List (Make_Others_Choice (Loc)),
          Statements       => New_List (Make_Null_Statement (Loc))));

      RPC_Receiver_Subp_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));

      RPC_Receiver_Declarations := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => RPC_Receiver_Subp_Id,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc)));

      RPC_Receiver_Statements := New_List (
        Make_Attribute_Reference (Loc,
          Prefix         =>
            New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc),
          Attribute_Name =>
            Name_Read,
          Expressions    => New_List (
            New_Occurrence_Of (Stub_Elements.RPC_Receiver_Stream, Loc),
            New_Occurrence_Of (RPC_Receiver_Subp_Id, Loc))));

      Append_To (RPC_Receiver_Statements,
        Make_Case_Statement (Loc,
          Expression   =>
            New_Occurrence_Of (RPC_Receiver_Subp_Id, Loc),
          Alternatives => RPC_Receiver_Case_Alternatives));

      RPC_Receiver_Decl :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Specification (Loc,
              Parent (Stub_Elements.Object_RPC_Receiver)),
          Declarations               => RPC_Receiver_Declarations,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => RPC_Receiver_Statements));

      Append_To (Decls, RPC_Receiver_Decl);

      --  Do not analyze RPC receiver at this stage since it will otherwise
      --  reference subprograms that have not been analyzed yet. It will
      --  be analyzed in the regular flow.

   end Add_RACW_Primitive_Declarations_And_Bodies;

   -----------------------------
   -- Add_RACW_Read_Attribute --
   -----------------------------

   procedure Add_RACW_Read_Attribute
     (RACW_Type           : Entity_Id;
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Declarations        : List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

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

      --  If the RACW denotes an object created on the current partition, then
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
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Origin)),
          Expression =>
            New_Occurrence_Of (Source_Partition, Loc)),

        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Receiver)),
          Expression =>
            New_Occurrence_Of (Source_Receiver, Loc)),

        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Addr)),
          Expression =>
            New_Occurrence_Of (Source_Address, Loc)));

      Append_To (Remote_Statements,
        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Asynchronous)),
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
                  Name =>
                    New_Occurrence_Of (RTE (RE_Get_Local_Partition_Id), Loc)),
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

   ------------------------------------
   -- Add_RACW_Read_Write_Attributes --
   ------------------------------------

   procedure Add_RACW_Read_Write_Attributes
     (RACW_Type           : Entity_Id;
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Object_RPC_Receiver : Entity_Id;
      Declarations        : List_Id)
   is
   begin
      Add_RACW_Write_Attribute
        (RACW_Type           => RACW_Type,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Object_RPC_Receiver => Object_RPC_Receiver,
         Declarations        => Declarations);

      Add_RACW_Read_Attribute
        (RACW_Type        => RACW_Type,
         Stub_Type        => Stub_Type,
         Stub_Type_Access => Stub_Type_Access,
         Declarations     => Declarations);
   end Add_RACW_Read_Write_Attributes;

   ------------------------------
   -- Add_RACW_Write_Attribute --
   ------------------------------

   procedure Add_RACW_Write_Attribute
     (RACW_Type           : Entity_Id;
      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Object_RPC_Receiver : Entity_Id;
      Declarations        : List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

      Is_RAS : constant Boolean := not Comes_From_Source (RACW_Type);

      Body_Node : Node_Id;
      Proc_Decl : Node_Id;
      Attr_Decl : Node_Id;

      RPC_Receiver : Node_Id;

      Statements        : List_Id;
      Local_Statements  : List_Id;
      Remote_Statements : List_Id;
      Null_Statements   : List_Id;

      Procedure_Name    : constant Name_Id := New_Internal_Name ('R');

      --  Functions to create occurrences of the formal
      --  parameter names.

      function Stream_Parameter return Node_Id;
      function Object return Node_Id;

      function Stream_Parameter return Node_Id is
      begin
         return Make_Identifier (Loc, Name_S);
      end Stream_Parameter;

      function Object return Node_Id is
      begin
         return Make_Identifier (Loc, Name_V);
      end Object;

   begin
      --  Build the code fragment corresponding to the marshalling of a
      --  local object.

      if Is_RAS then

         --  For a RAS, the RPC receiver is that of the RCI unit,
         --  not that of the corresponding distributed object type.
         --  We retrieve its address from the local proxy object.

         RPC_Receiver := Make_Selected_Component (Loc,
           Prefix         =>
             Unchecked_Convert_To (RTE (RE_RAS_Proxy_Type_Access), Object),
           Selector_Name =>
             Make_Identifier (Loc, Name_Receiver));

      else
         RPC_Receiver := Make_Attribute_Reference (Loc,
           Prefix         =>
             New_Occurrence_Of (Object_RPC_Receiver, Loc),
           Attribute_Name =>
             Name_Address);
      end if;

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

      --  Build the code fragment corresponding to the marshalling of a null
      --  object.

      Null_Statements := New_List (

        Pack_Entity_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => RTE (RE_Get_Local_Partition_Id)),

        Pack_Node_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => OK_Convert_To (RTE (RE_Unsigned_64),
            Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Object_RPC_Receiver, Loc),
              Attribute_Name => Name_Address)),
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
      --  Ras_Type is the access to subprogram type while Fat_Type points to
      --  the record type corresponding to a remote access to subprogram type.

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

      Proc_Decls        : List_Id;
      Proc_Statements   : List_Id;

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
                 Prefix        => New_Occurrence_Of (Stub_Ptr, Loc),
                 Selector_Name => Make_Identifier (Loc, Field_Name)),
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
      --    if Origin = Local_Partition_Id and then not All_Calls_Remote then
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

      --  Set the kind and return type of the function to prevent ambiguities
      --  between Ras_Type and Fat_Type in subsequent analysis.

      Set_Ekind (Proc, E_Function);
      Set_Etype (Proc, New_Occurrence_Of (Fat_Type, Loc));

      Discard_Node (
        Make_Subprogram_Body (Loc,
          Specification              => Proc_Spec,
          Declarations               => Proc_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Proc_Statements)));

      Set_TSS (Fat_Type, Proc);
   end Add_RAS_Access_TSS;

   -----------------------------
   -- Add_RAS_Dereference_TSS --
   -----------------------------

   --  This subprogram could use more comments ???

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

   begin
      Param_Specs := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => RAS_Parameter,
          In_Present          => True,
          Parameter_Type      =>
            New_Occurrence_Of (Fat_Type, Loc)));

      Is_Degenerate := False;
      Current_Parameter := First (Parameter_Specifications (Type_Def));
      Parameters : while Current_Parameter /= Empty loop
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

         --  Generate a dummy body recursing on the Dereference TSS, since
         --  actually it will never be executed.

         Append_To (Stmts,
           Make_Raise_Program_Error (Loc, Reason => PE_Explicit_Raise));
         RACW_Primitive_Name := New_Occurrence_Of (Proc, Loc);

      else
         Prepend_To (Param_Assoc,
           Unchecked_Convert_To (RACW_Type,
             New_Occurrence_Of (RAS_Parameter, Loc)));

         RACW_Primitive_Name :=
           Make_Selected_Component (Loc,
             Prefix =>
               New_Occurrence_Of (Scope (RACW_Type), Loc),
             Selector_Name =>
               Make_Identifier (Loc, Name_Call));
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
     (Decls              :     List_Id;
      Vis_Decl           :     Node_Id;
      All_Calls_Remote_E :     Entity_Id;
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
      Next (Formal);

      while Present (Formal) loop
         Append_To (Actuals, New_Occurrence_Of (
           Defining_Identifier (Formal), Loc));
         Next (Formal);
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
   begin
      --  Do not add attributes more than once in any case. This should
      --  be replaced by an assert or this comment removed if we decide
      --  that this is normal to be called several times ???

      if Present (TSS (Equivalent_Type (Defining_Identifier (Vis_Decl)),
                       TSS_RAS_Access))
      then
         return;
      end if;

      Add_RAS_Dereference_TSS (Vis_Decl);
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

      Stream_Parameter : Node_Id;
      Result_Parameter : Node_Id;

      Pkg_RPC_Receiver            : Node_Id;
      Pkg_RPC_Receiver_Spec       : Node_Id;
      Pkg_RPC_Receiver_Decls      : List_Id;
      Pkg_RPC_Receiver_Statements : List_Id;
      Pkg_RPC_Receiver_Cases      : constant List_Id := New_List;
      Pkg_RPC_Receiver_Body       : Node_Id;
      --  A Pkg_RPC_Receiver is built to decode the request

      Lookup_RAS_Info : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
      --  A remote subprogram is created to allow peers to look up
      --  RAS information using subprogram ids.

      Subp_Id : Node_Id;
      --  Subprogram_Id as read from the incoming stream

      Current_Declaration       : Node_Id;
      Current_Subprogram_Number : Int := First_RCI_Subprogram_Id;
      Current_Stubs             : Node_Id;

      Subp_Info_Array : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('I'));

      Subp_Info_List : constant List_Id := New_List;

      Dummy_Register_Name : Name_Id;
      Dummy_Register_Spec : Node_Id;
      Dummy_Register_Decl : Node_Id;
      Dummy_Register_Body : Node_Id;

      All_Calls_Remote_E  : Entity_Id;
      Proxy_Object_Addr   : Entity_Id;

      procedure Append_Stubs_To
        (RPC_Receiver_Cases : List_Id;
         Declaration        : Node_Id;
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
         Declaration        : Node_Id;
         Stubs              : Node_Id;
         Subprogram_Number  : Int)
      is
         Actuals : constant List_Id :=
                     New_List (New_Occurrence_Of (Stream_Parameter, Loc));
      begin
         if Nkind (Specification (Declaration)) = N_Function_Specification
           or else not
             Is_Asynchronous (Defining_Entity (Specification (Declaration)))
         then
            --  An asynchronous procedure does not want an output parameter
            --  since no result and no exception will ever be returned.

            Append_To (Actuals,
              New_Occurrence_Of (Result_Parameter, Loc));
         end if;

         Append_To (RPC_Receiver_Cases,
           Make_Case_Statement_Alternative (Loc,
             Discrete_Choices =>
                New_List (
                  Make_Integer_Literal (Loc, Subprogram_Number)),

             Statements       =>
               New_List (
                 Make_Procedure_Call_Statement (Loc,
                   Name                   =>
                     New_Occurrence_Of (
                       Defining_Entity (Stubs), Loc),
                   Parameter_Associations =>
                     Actuals))));
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

      Stream_Parameter :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Result_Parameter :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
      Subp_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

      Pkg_RPC_Receiver :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

      --  The parameters of the package RPC receiver are made of two
      --  streams, an input one and an output one.

      Pkg_RPC_Receiver_Spec :=
        Build_RPC_Receiver_Specification
          (RPC_Receiver     => Pkg_RPC_Receiver,
           Stream_Parameter => Stream_Parameter,
           Result_Parameter => Result_Parameter);

      Pkg_RPC_Receiver_Decls := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Subp_Id,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc)));

      Pkg_RPC_Receiver_Statements := New_List (
        Make_Attribute_Reference (Loc,
          Prefix         =>
            New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc),
          Attribute_Name =>
            Name_Read,
          Expressions    => New_List (
            New_Occurrence_Of (Stream_Parameter, Loc),
            New_Occurrence_Of (Subp_Id, Loc))));

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
                            New_Occurrence_Of (Stream_Parameter, Loc))))),
                  Selector_Name =>
                    Make_Identifier (Loc, Name_Subp_Id))))));

      All_Calls_Remote_E := Boolean_Literals (
        Has_All_Calls_Remote (Defining_Entity (Pkg_Spec)));

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
        Declaration =>
          Current_Declaration,
        Stubs       =>
          Current_Stubs,
        Subprogram_Number => 1);

      --  For each subprogram, the receiving stub will be built and a
      --  case statement will be made on the Subprogram_Id to dispatch
      --  to the right subprogram.

      Current_Declaration := First (Visible_Declarations (Pkg_Spec));
      while Current_Declaration /= Empty loop
         if Nkind (Current_Declaration) = N_Subprogram_Declaration
           and then Comes_From_Source (Current_Declaration)
         then
            pragma Assert (Current_Subprogram_Number =
              Get_Subprogram_Id (Defining_Unit_Name (Specification (
                Current_Declaration))));

            --  Build receiving stub

            Current_Stubs :=
              Build_Subprogram_Receiving_Stubs
                (Vis_Decl     => Current_Declaration,
                 Asynchronous =>
                   Nkind (Specification (Current_Declaration)) =
                       N_Procedure_Specification
                     and then Is_Asynchronous
                       (Defining_Unit_Name (Specification
                          (Current_Declaration))));

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
                          New_Occurrence_Of (Proxy_Object_Addr, Loc))))));

            Append_Stubs_To (Pkg_RPC_Receiver_Cases,
              Declaration =>
                Current_Declaration,
              Stubs =>
                Current_Stubs,
              Subprogram_Number =>
                Current_Subprogram_Number);
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

      Pkg_RPC_Receiver_Body :=
        Make_Subprogram_Body (Loc,
          Specification              => Pkg_RPC_Receiver_Spec,
          Declarations               => Pkg_RPC_Receiver_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Pkg_RPC_Receiver_Statements));

      Append_To (Decls, Pkg_RPC_Receiver_Body);
      Analyze (Pkg_RPC_Receiver_Body);

      --  Construction of the dummy package used to register the package
      --  receiving stubs on the nameserver.

      Dummy_Register_Name := New_Internal_Name ('P');

      Dummy_Register_Spec :=
        Make_Package_Specification (Loc,
          Defining_Unit_Name   =>
            Make_Defining_Identifier (Loc, Dummy_Register_Name),
          Visible_Declarations => No_List,
          End_Label => Empty);

      Dummy_Register_Decl :=
        Make_Package_Declaration (Loc,
          Specification => Dummy_Register_Spec);

      Append_To (Decls,
        Dummy_Register_Decl);
      Analyze (Dummy_Register_Decl);

      Dummy_Register_Body :=
        Make_Package_Body (Loc,
          Defining_Unit_Name         =>
            Make_Defining_Identifier (Loc, Dummy_Register_Name),
          Declarations               => No_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Procedure_Call_Statement (Loc,
                  Name                   =>
                    New_Occurrence_Of (RTE (RE_Register_Receiving_Stub), Loc),

                  Parameter_Associations => New_List (
                    Make_String_Literal (Loc,
                      Strval => Get_Pkg_Name_String_Id (Pkg_Spec)),
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of (Pkg_RPC_Receiver, Loc),
                      Attribute_Name =>
                        Name_Unrestricted_Access),
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of (Defining_Entity (Pkg_Spec), Loc),
                      Attribute_Name =>
                        Name_Version),
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        New_Occurrence_Of (Subp_Info_Array, Loc),
                      Attribute_Name =>
                        Name_Address),
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        New_Occurrence_Of (Subp_Info_Array, Loc),
                      Attribute_Name =>
                        Name_Length))))));

      Append_To (Decls, Dummy_Register_Body);
      Analyze (Dummy_Register_Body);
   end Add_Receiving_Stubs_To_Declarations;

   -------------------
   -- Add_Stub_Type --
   -------------------

   procedure Add_Stub_Type
     (Designated_Type     : Entity_Id;
      RACW_Type           : Entity_Id;
      Decls               : List_Id;
      Stub_Type           : out Entity_Id;
      Stub_Type_Access    : out Entity_Id;
      Object_RPC_Receiver : out Entity_Id;
      Existing            : out Boolean)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

      Stub_Elements : constant Stub_Structure :=
                        Stubs_Table.Get (Designated_Type);

      Stub_Type_Declaration           : Node_Id;
      Stub_Type_Access_Declaration    : Node_Id;
      Object_RPC_Receiver_Declaration : Node_Id;

      RPC_Receiver_Stream             : Entity_Id;
      RPC_Receiver_Result             : Entity_Id;

   begin
      if Stub_Elements /= Empty_Stub_Structure then
         Stub_Type           := Stub_Elements.Stub_Type;
         Stub_Type_Access    := Stub_Elements.Stub_Type_Access;
         Object_RPC_Receiver := Stub_Elements.Object_RPC_Receiver;
         Existing            := True;
         return;
      end if;

      Existing            := False;
      Stub_Type           :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Stub_Type_Access    :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Object_RPC_Receiver :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
      RPC_Receiver_Stream :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      RPC_Receiver_Result :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Stubs_Table.Set (Designated_Type,
        (Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Object_RPC_Receiver => Object_RPC_Receiver,
         RPC_Receiver_Stream => RPC_Receiver_Stream,
         RPC_Receiver_Result => RPC_Receiver_Result,
         RACW_Type           => RACW_Type));

      --  The stub type definition below must match exactly the one in
      --  s-parint.ads, since unchecked conversions will be used in
      --  s-parint.adb to modify pointers passed to Get_Unique_Remote_Pointer.

      Stub_Type_Declaration :=
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
                            New_Occurrence_Of (RTE (RE_Partition_ID), Loc))),

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
                            New_Occurrence_Of (Standard_Boolean, Loc)))))));

      Append_To (Decls, Stub_Type_Declaration);
      Analyze (Stub_Type_Declaration);

      --  This is in no way a type derivation, but we fake it to make
      --  sure that the dispatching table gets built with the corresponding
      --  primitive operations at the right place.

      Derive_Subprograms (Parent_Type  => Designated_Type,
                          Derived_Type => Stub_Type);

      Stub_Type_Access_Declaration :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Stub_Type_Access,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => True,
              Subtype_Indication => New_Occurrence_Of (Stub_Type, Loc)));

      Append_To (Decls, Stub_Type_Access_Declaration);
      Analyze (Stub_Type_Access_Declaration);

      Object_RPC_Receiver_Declaration :=
        Make_Subprogram_Declaration (Loc,
          Build_RPC_Receiver_Specification (
            RPC_Receiver     => Object_RPC_Receiver,
            Stream_Parameter => RPC_Receiver_Stream,
            Result_Parameter => RPC_Receiver_Result));

      Append_To (Decls, Object_RPC_Receiver_Declaration);
   end Add_Stub_Type;

   ---------------------------------
   -- Build_General_Calling_Stubs --
   ---------------------------------

   procedure Build_General_Calling_Stubs
     (Decls                     : List_Id;
      Statements                : List_Id;
      Target_Partition          : Entity_Id;
      RPC_Receiver              : Node_Id;
      Subprogram_Id             : Node_Id;
      Asynchronous              : Node_Id   := Empty;
      Is_Known_Asynchronous     : Boolean   := False;
      Is_Known_Non_Asynchronous : Boolean   := False;
      Is_Function               : Boolean;
      Spec                      : Node_Id;
      Stub_Type                 : Entity_Id := Empty;
      Nod                       : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      Stream_Parameter : Node_Id;
      --  Name of the stream used to transmit parameters to the remote package

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
      --  List of statements for extra formal parameters. It will appear after
      --  the regular statements for writing out parameters.

   begin
      --  The general form of a calling stub for a given subprogram is:

      --    procedure X (...) is
      --      P : constant Partition_ID := RCI_Cache.Get_Active_Partition_ID;
      --      Stream, Result : aliased System.RPC.Params_Stream_Type (0);
      --    begin
      --       Put_Package_RPC_Receiver_In_Stream; (the package RPC receiver
      --                  comes from RCI_Cache.Get_RCI_Package_Receiver)
      --       Put_Subprogram_Id_In_Stream;
      --       Put_Parameters_In_Stream;
      --       Do_RPC (Stream, Result);
      --       Read_Exception_Occurrence_From_Result; Raise_It;
      --       Read_Out_Parameters_And_Function_Return_From_Stream;
      --    end X;

      --  There are some variations: Do_APC is called for an asynchronous
      --  procedure and the part after the call is completely ommitted
      --  as well as the declaration of Result. For a function call,
      --  'Input is always used to read the result even if it is constrained.

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
            RPC_Receiver)));

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
      while Current_Parameter /= Empty loop
         declare
            Typ             : constant Node_Id :=
                                Parameter_Type (Current_Parameter);
            Etyp            : Entity_Id;
            Constrained     : Boolean;
            Value           : Node_Id;
            Extra_Parameter : Entity_Id;

         begin
            if Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type) then

               --  In the case of a controlling formal argument, we marshall
               --  its addr field rather than the local stub.

               Append_To (Statements,
                  Pack_Node_Into_Stream (Loc,
                    Stream => Stream_Parameter,
                    Object =>
                      Make_Selected_Component (Loc,
                        Prefix        =>
                          New_Occurrence_Of (
                            Defining_Identifier (Current_Parameter), Loc),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_Addr)),
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
                      Attribute_Name => Output_From_Constrained (Constrained),
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
               --  In this block, we do not use the extra formal that has been
               --  created because it does not exist at the time of expansion
               --  when building calling stubs for remote access to subprogram
               --  types. We create an extra variable of this type and push it
               --  in the stream after the regular parameters.

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
            --  Loop around parameters and assign out (or in out) parameters.
            --  In the case of RACW, controlling arguments cannot possibly
            --  have changed since they are remote, so we do not read them
            --  from the stream.

            Current_Parameter := First (Ordered_Parameters_List);
            while Current_Parameter /= Empty loop
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
         pragma Assert (Asynchronous /= Empty);
         Prepend_To (Asynchronous_Statements,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
             Attribute_Name => Name_Write,
             Expressions    => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         => New_Occurrence_Of (Stream_Parameter, Loc),
                 Attribute_Name => Name_Access),
               New_Occurrence_Of (Standard_True, Loc))));

         Prepend_To (Non_Asynchronous_Statements,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
             Attribute_Name => Name_Write,
             Expressions    => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         => New_Occurrence_Of (Stream_Parameter, Loc),
                 Attribute_Name => Name_Access),
               New_Occurrence_Of (Standard_False, Loc))));

         Append_To (Statements,
           Make_Implicit_If_Statement (Nod,
             Condition       => Asynchronous,
             Then_Statements => Asynchronous_Statements,
             Else_Statements => Non_Asynchronous_Statements));
      end if;
   end Build_General_Calling_Stubs;

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
                New_Occurrence_Of (Tag_Component
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
                  Make_Defining_Identifier (Loc,
                    Name_All_Calls_Remote),
                  Make_Component_Definition (Loc,
                    Subtype_Indication =>
                      New_Occurrence_Of (Standard_Boolean, Loc)),
                  ACR_Expression),

                Make_Component_Declaration (Loc,
                  Make_Defining_Identifier (Loc,
                    Name_Receiver),
                  Make_Component_Definition (Loc,
                    Subtype_Indication =>
                      New_Occurrence_Of (RTE (RE_Address), Loc)),
                  New_Occurrence_Of (RTE (RE_Null_Address), Loc)),

                Make_Component_Declaration (Loc,
                  Make_Defining_Identifier (Loc,
                    Name_Subp_Id),
                  Make_Component_Definition (Loc,
                    Subtype_Indication =>
                      New_Occurrence_Of (RTE (RE_Subprogram_Id), Loc))))));
   end Build_Remote_Subprogram_Proxy_Type;

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
      while Current_Parameter /= Empty loop
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

      Reg :=
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Register_Passive_Package), Loc),
          Parameter_Associations => New_List (
            Make_String_Literal (Loc, Get_Pkg_Name_String_Id (Pkg_Spec)),
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
     (RPC_Receiver     : Entity_Id;
      Stream_Parameter : Entity_Id;
      Result_Parameter : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (RPC_Receiver);

   begin
      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => RPC_Receiver,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Stream_Parameter,
              Parameter_Type      =>
                Make_Access_Definition (Loc,
                  Subtype_Mark =>
                    New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc))),

            Make_Parameter_Specification (Loc,
              Defining_Identifier => Result_Parameter,
              Parameter_Type      =>
                Make_Access_Definition (Loc,
                  Subtype_Mark =>
                    New_Occurrence_Of
                      (RTE (RE_Params_Stream_Type), Loc)))));
   end Build_RPC_Receiver_Specification;

   ------------------------------------
   -- Build_Subprogram_Calling_Stubs --
   ------------------------------------

   function Build_Subprogram_Calling_Stubs
     (Vis_Decl                 : Node_Id;
      Subp_Id                  : Int;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      Locator                  : Entity_Id := Empty;
      New_Name                 : Name_Id   := No_Name) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Vis_Decl);

      Target_Partition : Node_Id;
      --  Contains the name of the target partition

      Decls      : constant List_Id := New_List;
      Statements : constant List_Id := New_List;

      Subp_Spec : Node_Id;
      --  The specification of the body

      Controlling_Parameter : Entity_Id := Empty;
      RPC_Receiver          : Node_Id;

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
         Parameter_Entity  : constant Entity_Id :=
                               Defining_Identifier (Parameter);
         Condition         : Node_Id;

         Designated_Object : Node_Id;
         pragma Warnings (Off, Designated_Object);
         --  Is it really right that this is unreferenced ???

      begin
         --  The expression that will be built is of the form:
         --    if not (Parameter in Stub_Type and then
         --            Parameter.Origin = Controlling.Origin)
         --    then
         --      raise Constraint_Error;
         --    end if;

         --  Condition contains the reversed condition. Also, Parameter is
         --  dereferenced if it is an access type. We do not check that
         --  Parameter is in Stub_Type since such a check has been inserted
         --  at the point of call already (a tag check since we have multiple
         --  controlling operands).

         if Nkind (Parameter_Type (Parameter)) = N_Access_Definition then
            Designated_Object :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Parameter_Entity, Loc));
         else
            Designated_Object := New_Occurrence_Of (Parameter_Entity, Loc);
         end if;

         Condition :=
           Make_Op_Eq (Loc,
             Left_Opnd  =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   New_Occurrence_Of (Parameter_Entity, Loc),
               Selector_Name =>
                 Make_Identifier (Loc, Name_Origin)),

             Right_Opnd =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   New_Occurrence_Of (Controlling_Parameter, Loc),
               Selector_Name =>
                 Make_Identifier (Loc, Name_Origin)));

         Append_To (Decls,
           Make_Raise_Constraint_Error (Loc,
             Condition       =>
               Make_Op_Not (Loc, Right_Opnd => Condition),
             Reason => CE_Partition_Check_Failed));
      end Insert_Partition_Check;

   --  Start of processing for Build_Subprogram_Calling_Stubs

   begin
      Target_Partition :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

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

      if Stub_Type /= Empty
         and then Present (Parameter_Specifications (Spec_To_Use))
      then
         declare
            Current_Parameter : Node_Id :=
                                  First (Parameter_Specifications
                                           (Spec_To_Use));
         begin
            while Current_Parameter /= Empty loop

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

      if Stub_Type /= Empty then
         pragma Assert (Controlling_Parameter /= Empty);

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Target_Partition,
             Constant_Present    => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Partition_ID), Loc),

             Expression          =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   New_Occurrence_Of (Controlling_Parameter, Loc),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Origin))));

         RPC_Receiver :=
           Make_Selected_Component (Loc,
             Prefix        =>
               New_Occurrence_Of (Controlling_Parameter, Loc),
             Selector_Name =>
               Make_Identifier (Loc, Name_Receiver));

      else
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Target_Partition,
             Constant_Present    => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Partition_ID), Loc),

             Expression          =>
               Make_Function_Call (Loc,
                 Name => Make_Selected_Component (Loc,
                   Prefix        =>
                     Make_Identifier (Loc, Chars (RCI_Locator)),
                   Selector_Name =>
                     Make_Identifier (Loc, Name_Get_Active_Partition_ID)))));

         RPC_Receiver :=
           Make_Selected_Component (Loc,
             Prefix        =>
               Make_Identifier (Loc, Chars (RCI_Locator)),
             Selector_Name =>
               Make_Identifier (Loc, Name_Get_RCI_Package_Receiver));
      end if;

      if Dynamically_Asynchronous then
         Asynchronous_Expr :=
           Make_Selected_Component (Loc,
             Prefix        =>
               New_Occurrence_Of (Controlling_Parameter, Loc),
             Selector_Name =>
               Make_Identifier (Loc, Name_Asynchronous));
      end if;

      Build_General_Calling_Stubs
        (Decls                 => Decls,
         Statements            => Statements,
         Target_Partition      => Target_Partition,
         RPC_Receiver          => RPC_Receiver,
         Subprogram_Id         => Make_Integer_Literal (Loc, Subp_Id),
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
      return Make_Integer_Literal (Loc, Get_Subprogram_Id (E));
   end Build_Subprogram_Id;

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

      Stream_Parameter : Node_Id;
      Result_Parameter : Node_Id;
      --  See explanations of those in Build_Subprogram_Calling_Stubs

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

      Excep_Handler : Node_Id;
      Excep_Choice  : Entity_Id;
      Excep_Code    : List_Id;

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
      if RACW_Type /= Empty then
         Called_Subprogram :=
           New_Occurrence_Of (Parent_Primitive, Loc);
      else
         Called_Subprogram :=
           New_Occurrence_Of (
             Defining_Unit_Name (Specification (Vis_Decl)), Loc);
      end if;

      Stream_Parameter :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));

      if Dynamically_Asynchronous then
         Dynamic_Async :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      else
         Dynamic_Async := Empty;
      end if;

      if not Asynchronous or else Dynamically_Asynchronous then
         Result_Parameter :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('S'));

         --  The first statement after the subprogram call is a statement to
         --  writes a Null_Occurrence into the result stream.

         Null_Raise_Statement :=
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),
             Attribute_Name => Name_Write,
             Expressions    => New_List (
               New_Occurrence_Of (Result_Parameter, Loc),
               New_Occurrence_Of (RTE (RE_Null_Occurrence), Loc)));

         if Dynamically_Asynchronous then
            Null_Raise_Statement :=
              Make_Implicit_If_Statement (Vis_Decl,
                Condition       =>
                  Make_Op_Not (Loc, New_Occurrence_Of (Dynamic_Async, Loc)),
                Then_Statements => New_List (Null_Raise_Statement));
         end if;

         Append_To (After_Statements, Null_Raise_Statement);

      else
         Result_Parameter := Empty;
      end if;

      --  Loop through every parameter and get its value from the stream. If
      --  the parameter is unconstrained, then the parameter is read using
      --  'Input at the point of declaration.

      Current_Parameter := First (Ordered_Parameters_List);

      while Current_Parameter /= Empty loop

         declare
            Etyp        : Entity_Id;
            RACW_Controlling : Boolean;
            Constrained : Boolean;
            Object      : Entity_Id;
            Expr        : Node_Id := Empty;

         begin
            Object := Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
            Set_Ekind (Object, E_Variable);

            RACW_Controlling :=
              Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type);

            if RACW_Controlling then

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
              or else RACW_Controlling
            then
               --  If an input parameter is contrained, then its reading is
               --  deferred until the beginning of the subprogram body. If
               --  it is unconstrained, then an expression is built for
               --  the object declaration and the variable is set using
               --  'Input instead of 'Read.

               if Constrained and then not RACW_Controlling then
                  Append_To (Statements,
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Etyp, Loc),
                      Attribute_Name => Name_Read,
                      Expressions    => New_List (
                        New_Occurrence_Of (Stream_Parameter, Loc),
                        New_Occurrence_Of (Object, Loc))));

               else
                  Expr := Input_With_Tag_Check (Loc,
                    Var_Type => Etyp,
                    Stream   => Stream_Parameter);
                  Append_To (Decls, Expr);
                  Expr := Make_Function_Call (Loc,
                    New_Occurrence_Of (Defining_Unit_Name
                      (Specification (Expr)), Loc));
               end if;
            end if;

            --  If we do not have to output the current parameter, then
            --  it can well be flagged as constant. This may allow further
            --  optimizations done by the back end.

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Object,
                Constant_Present    =>
                  not Constrained and then not Out_Present (Current_Parameter),
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
                       New_Occurrence_Of (Result_Parameter, Loc),
                     New_Occurrence_Of (Object, Loc))));
            end if;

            if
              Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type)
            then
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
                      Prefix         => New_Occurrence_Of (Formal_Type, Loc),
                      Attribute_Name => Name_Read,
                      Expressions    => New_List (
                        New_Occurrence_Of (Stream_Parameter, Loc),
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
         --  be able to hold a potentially unconstrained result in a variable.

         declare
            Etyp   : constant Entity_Id :=
                       Etype (Subtype_Mark (Specification (Vis_Decl)));
            Result : constant Node_Id   :=
                       Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

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
                  New_Occurrence_Of (Result_Parameter, Loc),
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
                  New_Occurrence_Of (Stream_Parameter, Loc),
                  New_Occurrence_Of (Dynamic_Async, Loc))));
         end if;

         Append_To (Statements,
           Make_Procedure_Call_Statement (Loc,
             Name                   => Called_Subprogram,
             Parameter_Associations => Parameter_List));

         Append_List_To (Statements, After_Statements);
      end if;

      if Asynchronous and then not Dynamically_Asynchronous then

         --  An asynchronous procedure does not want a Result
         --  parameter. Also, we put an exception handler with an others
         --  clause that does nothing.

         Subp_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       =>
               Make_Defining_Identifier (Loc, New_Internal_Name ('F')),
             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Stream_Parameter,
                 Parameter_Type      =>
                   Make_Access_Definition (Loc,
                   Subtype_Mark =>
                     New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc)))));

         Excep_Handler :=
           Make_Exception_Handler (Loc,
             Exception_Choices =>
               New_List (Make_Others_Choice (Loc)),
             Statements        => New_List (
               Make_Null_Statement (Loc)));

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
               New_Occurrence_Of (Result_Parameter, Loc),
               New_Occurrence_Of (Excep_Choice, Loc))));

         if Dynamically_Asynchronous then
            Excep_Code := New_List (
              Make_Implicit_If_Statement (Vis_Decl,
                Condition       => Make_Op_Not (Loc,
                  New_Occurrence_Of (Dynamic_Async, Loc)),
                Then_Statements => Excep_Code));
         end if;

         Excep_Handler :=
           Make_Exception_Handler (Loc,
             Choice_Parameter   => Excep_Choice,
             Exception_Choices  => New_List (Make_Others_Choice (Loc)),
             Statements         => Excep_Code);

         Subp_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       =>
               Make_Defining_Identifier (Loc, New_Internal_Name ('F')),

             Parameter_Specifications => New_List (
               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Stream_Parameter,
                 Parameter_Type      =>
                   Make_Access_Definition (Loc,
                   Subtype_Mark =>
                     New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc))),

               Make_Parameter_Specification (Loc,
                 Defining_Identifier => Result_Parameter,
                 Parameter_Type      =>
                   Make_Access_Definition (Loc,
                  Subtype_Mark =>
                  New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc)))));
      end if;

      return
        Make_Subprogram_Body (Loc,
          Specification              => Subp_Spec,
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements         => Statements,
              Exception_Handlers => New_List (Excep_Handler)));
   end Build_Subprogram_Receiving_Stubs;

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
         while Current_Parameter /= Empty loop
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

               if Object_Type /= Empty
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
         while Current_Parameter /= Empty loop
            if Out_Present (Current_Parameter) then
               return False;
            end if;

            Next (Current_Parameter);
         end loop;
      end if;

      return True;
   end Could_Be_Asynchronous;

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
            Subp_Id                => Get_Subprogram_Id (Called_Subprogram),
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
      Add_Calling_Stubs_To_Declarations (Specification (Unit_Node),
                                         Decls);
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
         Decls := Visible_Declarations (Spec);
         New_Scope (Scope_Of_Spec (Spec));
         Add_Receiving_Stubs_To_Declarations (Spec, Decls);

      else
         Spec  :=
           Package_Specification_Of_Scope (Corresponding_Spec (Unit_Node));
         Decls := Declarations (Unit_Node);
         New_Scope (Scope_Of_Spec (Unit_Node));
         Temp := New_List;
         Add_Receiving_Stubs_To_Declarations (Spec, Temp);
         Insert_List_Before (First (Decls), Temp);
      end if;

      Pop_Scope;
   end Expand_Receiving_Stubs_Bodies;

   ----------------------------
   -- Get_Pkg_Name_string_Id --
   ----------------------------

   function Get_Pkg_Name_String_Id (Decl_Node : Node_Id) return String_Id is
      Unit_Name_Id : constant Unit_Name_Type := Get_Unit_Name (Decl_Node);

   begin
      Get_Unit_Name_String (Unit_Name_Id);

      --  Remove seven last character (" (spec)" or " (body)").

      Name_Len := Name_Len - 7;
      pragma Assert (Name_Buffer (Name_Len + 1) = ' ');

      return Get_String_Id (Name_Buffer (1 .. Name_Len));
   end Get_Pkg_Name_String_Id;

   -------------------
   -- Get_String_Id --
   -------------------

   function Get_String_Id (Val : String) return String_Id is
   begin
      Start_String;
      Store_String_Chars (Val);
      return End_String;
   end Get_String_Id;

   -----------------------
   -- Get_Subprogram_Id --
   -----------------------

   function Get_Subprogram_Id (E : Entity_Id) return Int is
      Current_Declaration : Node_Id;
      Result              : Int := First_RCI_Subprogram_Id;

   begin
      pragma Assert
        (Is_Remote_Call_Interface (Scope (E))
           and then
             (Nkind (Parent (E)) = N_Procedure_Specification
                or else
              Nkind (Parent (E)) = N_Function_Specification));

      Current_Declaration :=
        First (Visible_Declarations
          (Package_Specification_Of_Scope (Scope (E))));

      while Current_Declaration /= Empty loop
         if Nkind (Current_Declaration) = N_Subprogram_Declaration
           and then Comes_From_Source (Current_Declaration)
         then
            if Defining_Unit_Name
                 (Specification (Current_Declaration)) = E
            then
               return Result;
            end if;

            Result := Result + 1;
         end if;

         Next (Current_Declaration);
      end loop;

      --  Error if we do not find it

      raise Program_Error;
   end Get_Subprogram_Id;

   ----------
   -- Hash --
   ----------

   function Hash (F : Entity_Id) return Hash_Index is
   begin
      return Hash_Index (Natural (F) mod Positive (Hash_Index'Last + 1));
   end Hash;

   --------------------------
   -- Input_With_Tag_Check --
   --------------------------

   function Input_With_Tag_Check
     (Loc      : Source_Ptr;
      Var_Type : Entity_Id;
      Stream   : Entity_Id)
      return     Node_Id
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
                      New_List (New_Occurrence_Of (Stream, Loc))))))));
   end Input_With_Tag_Check;

   --------------------------------
   -- Is_RACW_Controlling_Formal --
   --------------------------------

   function Is_RACW_Controlling_Formal
     (Parameter : Node_Id;
      Stub_Type : Entity_Id)
      return      Boolean
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
      if Etyp /= Empty then
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
      Inst : constant Node_Id :=
               Make_Package_Instantiation (Loc,
                 Defining_Unit_Name   =>
                   Make_Defining_Identifier (Loc, New_Internal_Name ('R')),
                 Name                 =>
                   New_Occurrence_Of (RTE (RE_RCI_Info), Loc),
                 Generic_Associations => New_List (
                   Make_Generic_Association (Loc,
                     Selector_Name                     =>
                       Make_Identifier (Loc, Name_RCI_Name),
                     Explicit_Generic_Actual_Parameter =>
                       Make_String_Literal (Loc,
                         Strval => Get_Pkg_Name_String_Id (Package_Spec)))));

   begin
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
            Parent (Declaration_Node (Stub_Elements.Object_RPC_Receiver)),
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
