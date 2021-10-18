------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 C U D A                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines CUDA-specific datastructures and functions.

with Atree;          use Atree;
with Debug;          use Debug;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Util;       use Sem_Util;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo;          use Sinfo;
with Snames;         use Snames;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;

with GNAT.HTable;

package body GNAT_CUDA is

   --------------------------------------
   -- Hash Table for CUDA_Global nodes --
   --------------------------------------

   type Hash_Range is range 0 .. 510;
   --  Size of hash table headers

   function Hash (F : Entity_Id) return Hash_Range;
   --  Hash function for hash table

   package CUDA_Device_Entities_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => Hash_Range,
        Element    => Elist_Id,
        No_Element => No_Elist,
        Key        => Entity_Id,
        Hash       => Hash,
        Equal      => "=");
   --  The keys of this table are package entities whose bodies contain at
   --  least one procedure marked with aspect CUDA_Device. The values are
   --  Elists of the marked entities.

   package CUDA_Kernels_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => Hash_Range,
        Element    => Elist_Id,
        No_Element => No_Elist,
        Key        => Entity_Id,
        Hash       => Hash,
        Equal      => "=");
   --  The keys of this table are package entities whose bodies contain at
   --  least one procedure marked with aspect CUDA_Global. The values are
   --  Elists of the marked procedures.

   procedure Build_And_Insert_CUDA_Initialization (N : Node_Id);
   --  Builds declarations necessary for CUDA initialization and inserts them
   --  in N, the package body that contains CUDA_Global nodes. These
   --  declarations are:
   --
   --    * A symbol to hold the pointer P to the CUDA fat binary.
   --
   --    * A type definition T for a wrapper that contains the pointer to the
   --      CUDA fat binary.
   --
   --    * An object of the aforementioned type to hold the aforementioned
   --      pointer.
   --
   --    * For each CUDA_Global procedure in the package, a declaration of a C
   --      string containing the function's name.
   --
   --    * A procedure that takes care of calling CUDA functions that register
   --      CUDA_Global procedures with the runtime.

   procedure Empty_CUDA_Global_Subprograms (Pack_Id : Entity_Id);
   --  For all subprograms marked CUDA_Global in Pack_Id, remove declarations
   --  and replace statements with a single null statement.
   --  This is required because CUDA_Global subprograms could be referring to
   --  device-only symbols, which would result in unknown symbols at link time
   --  if kept around.
   --  We choose to empty CUDA_Global subprograms rather than completely
   --  removing them from the package because registering CUDA_Global
   --  subprograms with the CUDA runtime on the host requires knowing the
   --  subprogram's host-side address.

   function Get_CUDA_Device_Entities (Pack_Id : Entity_Id) return Elist_Id;
   --  Returns an Elist of all entities marked with pragma CUDA_Device that
   --  are declared within package body Pack_Body. Returns No_Elist if Pack_Id
   --  does not contain such entities.

   function Get_CUDA_Kernels (Pack_Id : Entity_Id) return Elist_Id;
   --  Returns an Elist of all procedures marked with pragma CUDA_Global that
   --  are declared within package body Pack_Body. Returns No_Elist if Pack_Id
   --  does not contain such procedures.

   procedure Remove_CUDA_Device_Entities (Pack_Id : Entity_Id);
   --  Removes all entities marked with the CUDA_Device pragma from package
   --  Pack_Id. Must only be called when compiling for the host.

   procedure Set_CUDA_Device_Entities
     (Pack_Id : Entity_Id;
      E       : Elist_Id);
   --  Stores E as the list of CUDA_Device entities belonging to the package
   --  entity Pack_Id. Pack_Id must not have a list of device entities.

   procedure Set_CUDA_Kernels
     (Pack_Id : Entity_Id;
      Kernels : Elist_Id);
   --  Stores Kernels as the list of kernels belonging to the package entity
   --  Pack_Id. Pack_Id must not have a list of kernels.

   ----------------------------
   -- Add_CUDA_Device_Entity --
   ----------------------------

   procedure Add_CUDA_Device_Entity
     (Pack_Id : Entity_Id;
      E       : Entity_Id)
   is
      Device_Entities : Elist_Id := Get_CUDA_Device_Entities (Pack_Id);
   begin
      if Device_Entities = No_Elist then
         Device_Entities := New_Elmt_List;
         Set_CUDA_Device_Entities (Pack_Id, Device_Entities);
      end if;
      Append_Elmt (E, Device_Entities);
   end Add_CUDA_Device_Entity;

   ---------------------
   -- Add_CUDA_Kernel --
   ---------------------

   procedure Add_CUDA_Kernel
     (Pack_Id : Entity_Id;
      Kernel  : Entity_Id)
   is
      Kernels : Elist_Id := Get_CUDA_Kernels (Pack_Id);
   begin
      if Kernels = No_Elist then
         Kernels := New_Elmt_List;
         Set_CUDA_Kernels (Pack_Id, Kernels);
      end if;
      Append_Elmt (Kernel, Kernels);
   end Add_CUDA_Kernel;

   -----------------------------------
   -- Empty_CUDA_Global_Subprograms --
   -----------------------------------

   procedure Empty_CUDA_Global_Subprograms (Pack_Id : Entity_Id) is
      Spec_Id     : constant Node_Id := Corresponding_Spec (Pack_Id);
      Kernels     : constant Elist_Id := Get_CUDA_Kernels (Spec_Id);
      Kernel_Elm  : Elmt_Id;
      Kernel      : Entity_Id;
      Kernel_Body : Node_Id;
      Null_Body   : Entity_Id;
      Loc         : Source_Ptr;
   begin
      --  It is an error to empty CUDA_Global subprograms when not compiling
      --  for the host.
      pragma Assert (Debug_Flag_Underscore_C);

      if No (Kernels) then
         return;
      end if;

      Kernel_Elm := First_Elmt (Kernels);
      while Present (Kernel_Elm) loop
         Kernel := Node (Kernel_Elm);
         Kernel_Body := Subprogram_Body (Kernel);
         Loc := Sloc (Kernel_Body);

         Null_Body := Make_Subprogram_Body (Loc,
           Specification              => Subprogram_Specification (Kernel),
           Declarations               => New_List,
           Handled_Statement_Sequence =>
             Make_Handled_Sequence_Of_Statements (Loc,
               Statements => New_List (Make_Null_Statement (Loc))));

         Rewrite (Kernel_Body, Null_Body);

         Next_Elmt (Kernel_Elm);
      end loop;
   end Empty_CUDA_Global_Subprograms;

   -------------------------
   -- Expand_CUDA_Package --
   -------------------------

   procedure Expand_CUDA_Package (N : Node_Id) is
   begin

      --  If not compiling for the host, do not do anything.

      if not Debug_Flag_Underscore_C then
         return;
      end if;

      --  Remove the content (both declarations and statements) of CUDA_Global
      --  procedures. This is required because CUDA_Global functions could be
      --  referencing entities available only on the device, which would result
      --  in unknown symbol errors at link time.

      Empty_CUDA_Global_Subprograms (N);

      --  Remove CUDA_Device entities (except if they are also CUDA_Host), as
      --  they can only be referenced from the device and might reference
      --  device-only symbols.

      Remove_CUDA_Device_Entities
        (Package_Specification (Corresponding_Spec (N)));

      --  If procedures marked with CUDA_Global have been defined within N,
      --  we need to register them with the CUDA runtime at program startup.
      --  This requires multiple declarations and function calls which need
      --  to be appended to N's declarations.

      Build_And_Insert_CUDA_Initialization (N);
   end Expand_CUDA_Package;

   ----------
   -- Hash --
   ----------

   function Hash (F : Entity_Id) return Hash_Range is
   begin
      return Hash_Range (F mod 511);
   end Hash;

   ------------------------------
   -- Get_CUDA_Device_Entities --
   ------------------------------

   function Get_CUDA_Device_Entities (Pack_Id : Entity_Id) return Elist_Id is
   begin
      return CUDA_Device_Entities_Table.Get (Pack_Id);
   end Get_CUDA_Device_Entities;

   ----------------------
   -- Get_CUDA_Kernels --
   ----------------------

   function Get_CUDA_Kernels (Pack_Id : Entity_Id) return Elist_Id is
   begin
      return CUDA_Kernels_Table.Get (Pack_Id);
   end Get_CUDA_Kernels;

   ------------------------------------------
   -- Build_And_Insert_CUDA_Initialization --
   ------------------------------------------

   procedure Build_And_Insert_CUDA_Initialization (N : Node_Id) is

      --  For the following kernel declaration:
      --
      --  package body <Package_Name> is
      --     procedure <Proc_Name> (X : Integer) with CUDA_Global;
      --  end package;
      --
      --  Insert the following declarations:
      --
      --     Fat_Binary : System.Address;
      --     pragma Import
      --        (Convention    => C,
      --         Entity        => Fat_Binary,
      --         External_Name => "_binary_<Package_Name>_fatbin_start");
      --
      --     Wrapper : Fatbin_Wrapper :=
      --       (16#466243b1#, 1, Fat_Binary'Address, System.Null_Address);
      --
      --     Proc_Symbol_Name : Interfaces.C.Strings.Chars_Ptr :=
      --       Interfaces.C.Strings.New_Char_Array("<Proc_Name>");
      --
      --     Fat_Binary_Handle : System.Address :=
      --       CUDA.Internal.Register_Fat_Binary (Wrapper'Address);
      --
      --     procedure Initialize_CUDA_Kernel is
      --     begin
      --        CUDA.Internal.Register_Function
      --           (Fat_Binary_Handle,
      --            <Proc_Name>'Address,
      --            Proc_Symbol_Name,
      --            Proc_Symbol_Name,
      --            -1,
      --            System.Null_Address,
      --            System.Null_Address,
      --            System.Null_Address,
      --            System.Null_Address,
      --            System.Null_Address);
      --        CUDA.Internal.Register_Fat_Binary_End (Fat_Binary_Handle);
      --     end Initialize_CUDA_Kernel;
      --
      --  Proc_Symbol_Name is the name of the procedure marked with
      --  CUDA_Global. The CUDA runtime uses this in order to be able to find
      --  kernels in the fat binary, so it has to match the name of the
      --  procedure symbol compiled by GNAT_LLVM. When looking at the code
      --  generated by NVCC, it seems that the CUDA runtime also needs the name
      --  of the procedure symbol of the host. Fortuantely, the procedures are
      --  named the same way whether they are compiled for the host or the
      --  device, so we use Vector_Add_Name to specify the name of the symbol
      --  for both the host and the device. The meaning of the rest of the
      --  arguments is unknown.

      function Build_CUDA_Init_Proc
        (Init_Id      : Entity_Id;
         CUDA_Kernels : Elist_Id;
         Handle_Id    : Entity_Id;
         Pack_Decls   : List_Id) return Node_Id;
      --  Create the declaration of Init_Id, the function that binds each
      --  kernel present in CUDA_Kernels with the fat binary Handle_Id and then
      --  tells the CUDA runtime that no new function will be bound to the fat
      --  binary.

      function Build_Fat_Binary_Declaration
        (Bin_Id : Entity_Id) return Node_Id;
      --  Create a declaration for Bin_Id, the entity that represents the fat
      --  binary, i.e.:
      --
      --    Bin_Id : System.Address;

      function Build_Fat_Binary_Handle_Declaration
        (Handle_Id  : Entity_Id;
         Wrapper_Id : Entity_Id) return Node_Id;
      --  Create the declaration of Handle_Id, a System.Address that will
      --  receive the results of passing the address of Wrapper_Id to
      --  CUDA.Register_Fat_Binary, i.e.:
      --
      --    Handle_Id : System.Address :=
      --      CUDA.Register_Fat_Binary (Wrapper_Id'Address)

      function Build_Fat_Binary_Wrapper_Declaration
        (Wrapper_Id : Entity_Id;
         Bin_Id     : Entity_Id) return Node_Id;
      --  Create the declaration of the fat binary wrapper Wrapper_Id, which
      --  holds magic numbers and Bin_Id'Address, i.e.:
      --
      --     Wrapper_Id : System.Address :=
      --       (16#466243b1#, 1, Bin_Id'Address, System.Null_Address);

      function Build_Import_Pragma
        (Bin_Id    : Entity_Id;
         Pack_Body : Node_Id) return Node_Id;
      --  Create a pragma that will bind the fat binary Bin_Id to its external
      --  symbol. N is the package body Bin_Id belongs to, i.e.:
      --
      --     pragma Import
      --        (Convention    => C,
      --         Entity        => Bin_Id,
      --         External_Name => "_binary_<Pack_Body's name>_fatbin_start");

      function Build_Kernel_Name_Declaration
        (Kernel : Entity_Id) return Node_Id;
      --  Create the declaration of a C string that contains the name of
      --  Kernel's symbol, i.e.:
      --
      --     Kernel : Interfaces.C.Strings.Chars_Ptr :=
      --       Interfaces.C.Strings.New_Char_Array("<Kernel's name>");

      function Build_Register_Procedure_Call
        (Loc         : Source_Ptr;
         Bin         : Entity_Id;
         Kernel      : Entity_Id;
         Kernel_Name : Entity_Id) return Node_Id;
      --  Return a call to CUDA.Internal.Register_Function that binds Kernel
      --  (the entity of a procedure) to the symbol described by the C string
      --  Kernel_Name in the fat binary Bin, using Loc as location.

      --------------------------
      -- Build_CUDA_Init_Proc --
      --------------------------

      function Build_CUDA_Init_Proc
        (Init_Id      : Entity_Id;
         CUDA_Kernels : Elist_Id;
         Handle_Id    : Entity_Id;
         Pack_Decls   : List_Id) return Node_Id
      is
         Loc : constant Source_Ptr := Sloc (Init_Id);

         Stmts : constant List_Id := New_List;
         --  List of statements that will be used by the cuda initialization
         --  function.

         New_Stmt : Node_Id;
         --  Temporary variable to hold the various newly-created nodes

         Kernel_Elmt : Elmt_Id;
         Kernel_Id   : Entity_Id;

      begin
         --  For each CUDA_Global function, declare a C string that holds
         --  its symbol's name (i.e. packagename __ functionname).

         --  Also create a function call to CUDA.Internal.Register_Function
         --  that takes the declared C string, a pointer to the function and
         --  the fat binary handle.

         Kernel_Elmt := First_Elmt (CUDA_Kernels);
         while Present (Kernel_Elmt) loop
            Kernel_Id := Node (Kernel_Elmt);

            New_Stmt := Build_Kernel_Name_Declaration (Kernel_Id);
            Append (New_Stmt, Pack_Decls);
            Analyze (New_Stmt);

            Append_To (Stmts,
              Build_Register_Procedure_Call (Loc,
                Bin         => Handle_Id,
                Kernel      => Kernel_Id,
                Kernel_Name => Defining_Entity (New_Stmt)));

            Next_Elmt (Kernel_Elmt);
         end loop;

         --  Finish the CUDA initialization function: add a call to
         --  register_fat_binary_end, to let the CUDA runtime know that we
         --  won't be registering any other symbol with the current fat binary.

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Register_Fat_Binary_End), Loc),
             Parameter_Associations =>
               New_List (New_Occurrence_Of (Handle_Id, Loc))));

         --  Now that we have all the declarations and calls we need, we can
         --  build and and return the initialization procedure.

         return
           Make_Subprogram_Body (Loc,
             Specification              =>
               Make_Procedure_Specification (Loc, Init_Id),
             Declarations               => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc, Stmts));
      end Build_CUDA_Init_Proc;

      ----------------------------------
      -- Build_Fat_Binary_Declaration --
      ----------------------------------

      function Build_Fat_Binary_Declaration
        (Bin_Id : Entity_Id) return Node_Id
      is
      begin
         return
           Make_Object_Declaration (Sloc (Bin_Id),
             Defining_Identifier => Bin_Id,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Address), Sloc (Bin_Id)));
      end Build_Fat_Binary_Declaration;

      -----------------------------------------
      -- Build_Fat_Binary_Handle_Declaration --
      -----------------------------------------

      function Build_Fat_Binary_Handle_Declaration
        (Handle_Id  : Entity_Id;
         Wrapper_Id : Entity_Id) return Node_Id
      is
         Loc : constant Source_Ptr := Sloc (Handle_Id);
      begin
         --  Generate:
         --    Handle_Id : System.Address :=
         --      CUDA.Register_Fat_Binary (Wrapper_Id'Address);

         return
           Make_Object_Declaration (Loc,
             Defining_Identifier => Handle_Id,
             Object_Definition   => New_Occurrence_Of (RTE (RE_Address), Loc),
             Expression          =>
               Make_Function_Call (Loc,
                 Name                   =>
                   New_Occurrence_Of (RTE (RE_Register_Fat_Binary), Loc),
                 Parameter_Associations => New_List (
                   Make_Attribute_Reference (Loc,
                     Prefix         =>
                       New_Occurrence_Of (Wrapper_Id, Loc),
                     Attribute_Name => Name_Address))));
      end Build_Fat_Binary_Handle_Declaration;

      ------------------------------------------
      -- Build_Fat_Binary_Wrapper_Declaration --
      ------------------------------------------

      function Build_Fat_Binary_Wrapper_Declaration
        (Wrapper_Id : Entity_Id;
         Bin_Id     : Entity_Id) return Node_Id
      is
         Loc : constant Source_Ptr := Sloc (Wrapper_Id);
      begin
         return
           Make_Object_Declaration (Loc,
             Defining_Identifier => Wrapper_Id,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Fatbin_Wrapper), Loc),
             Expression          =>
               Make_Aggregate (Loc,
                 Expressions => New_List (
                   Make_Integer_Literal (Loc, UI_From_Int (16#466243b1#)),
                   Make_Integer_Literal (Loc, Uint_1),
                   Make_Attribute_Reference (Loc,
                     Prefix         => New_Occurrence_Of (Bin_Id, Loc),
                     Attribute_Name => Name_Address),
                   New_Occurrence_Of (RTE (RE_Null_Address), Loc))));
      end Build_Fat_Binary_Wrapper_Declaration;

      -------------------------
      -- Build_Import_Pragma --
      -------------------------

      function Build_Import_Pragma
        (Bin_Id    : Entity_Id;
         Pack_Body : Node_Id) return Node_Id
      is
         Loc             : constant Source_Ptr := Sloc (Bin_Id);
         External_Symbol : String_Id;
      begin
         Start_String;
         Store_String_Chars
           ("_binary_"
            & Get_Name_String (Chars (Defining_Unit_Name (Pack_Body)))
            & "_fatbin_start");
         External_Symbol := End_String;

         return
           Make_Pragma (Loc,
             Pragma_Identifier            =>
               Make_Identifier (Loc, Name_Import),
             Pragma_Argument_Associations => New_List (
               Make_Pragma_Argument_Association (Loc,
                 Chars      => Name_Convention,
                 Expression => Make_Identifier (Loc, Name_C)),
               Make_Pragma_Argument_Association (Loc,
                 Chars      => Name_Entity,
                 Expression => New_Occurrence_Of (Bin_Id, Loc)),
               Make_Pragma_Argument_Association (Loc,
                 Chars      => Name_External_Name,
                 Expression => Make_String_Literal (Loc, External_Symbol))));
      end Build_Import_Pragma;

      -------------------------------------
      -- Build_Kernel_Name_Declaration --
      -------------------------------------

      function Build_Kernel_Name_Declaration
        (Kernel : Entity_Id) return Node_Id
      is
         Loc : constant Source_Ptr := Sloc (Kernel);

         Package_Name : constant String :=
           Get_Name_String (Chars (Scope (Kernel)));

         Symbol_Name : constant String := Get_Name_String (Chars (Kernel));

         Kernel_Name : String_Id;
      begin
         Start_String;
         Store_String_Chars (Package_Name & "__" & Symbol_Name);
         Kernel_Name := End_String;

         return
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Temporary (Loc, 'C'),
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Chars_Ptr), Loc),
             Expression          =>
               Make_Function_Call (Loc,
                 Name                   =>
                   New_Occurrence_Of (RTE (RE_New_Char_Array), Loc),
                 Parameter_Associations => New_List (
                   Make_String_Literal (Loc, Kernel_Name))));
      end Build_Kernel_Name_Declaration;

      -----------------------------------
      -- Build_Register_Procedure_Call --
      -----------------------------------

      function Build_Register_Procedure_Call
        (Loc         : Source_Ptr;
         Bin         : Entity_Id;
         Kernel      : Entity_Id;
         Kernel_Name : Entity_Id) return Node_Id
      is
         Args : constant List_Id := New_List;
      begin
         --  First argument: the handle of the fat binary

         Append (New_Occurrence_Of (Bin, Loc), Args);

         --  Second argument: the host address of the function that is marked
         --  with CUDA_Global.

         Append_To (Args,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Kernel, Loc),
             Attribute_Name => Name_Address));

         --  Third argument, the name of the function on the host

         Append (New_Occurrence_Of (Kernel_Name, Loc), Args);

         --  Fourth argument, the name of the function on the device

         Append (New_Occurrence_Of (Kernel_Name, Loc), Args);

         --  Fith argument: -1. Meaning unknown - this has been copied from
         --  LLVM.

         Append (Make_Integer_Literal (Loc, Uint_Minus_1), Args);

         --  Args 6, 7, 8, 9, 10: Null pointers. Again, meaning unknown

         for Arg_Count in 6 .. 10 loop
            Append_To (Args, New_Occurrence_Of (RTE (RE_Null_Address), Loc));
         end loop;

         --  Build the call to CUDARegisterFunction, passing the argument list
         --  we just built.

         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Register_Function), Loc),
             Parameter_Associations => Args);
      end Build_Register_Procedure_Call;

      --  Local declarations

      Loc : constant Source_Ptr := Sloc (N);

      Spec_Id : constant Node_Id := Corresponding_Spec (N);
      --  The specification of the package we're adding a cuda init func to

      Pack_Decls : constant List_Id := Declarations (N);

      CUDA_Node_List : constant Elist_Id := Get_CUDA_Kernels (Spec_Id);
      --  CUDA nodes that belong to the package

      CUDA_Init_Func : Entity_Id;
      --  Entity of the cuda init func

      Fat_Binary : Entity_Id;
      --  Entity of the fat binary of N. Bound to said fat binary by a pragma

      Fat_Binary_Handle : Entity_Id;
      --  Entity of the result of passing the fat binary wrapper to
      --  CUDA.Register_Fat_Binary.

      Fat_Binary_Wrapper : Entity_Id;
      --  Entity of a record that holds a bunch of magic numbers and a
      --  reference to Fat_Binary.

      New_Stmt : Node_Id;
      --  Node to store newly-created declarations

   --  Start of processing for Build_And_Insert_CUDA_Initialization

   begin
      if CUDA_Node_List = No_Elist then
         return;
      end if;

      Fat_Binary := Make_Temporary (Loc, 'C');
      New_Stmt := Build_Fat_Binary_Declaration (Fat_Binary);
      Append_To (Pack_Decls, New_Stmt);
      Analyze (New_Stmt);

      New_Stmt := Build_Import_Pragma (Fat_Binary, N);
      Append_To (Pack_Decls, New_Stmt);
      Analyze (New_Stmt);

      Fat_Binary_Wrapper := Make_Temporary (Loc, 'C');
      New_Stmt :=
        Build_Fat_Binary_Wrapper_Declaration
          (Wrapper_Id => Fat_Binary_Wrapper,
           Bin_Id     => Fat_Binary);
      Append_To (Pack_Decls, New_Stmt);
      Analyze (New_Stmt);

      Fat_Binary_Handle := Make_Temporary (Loc, 'C');
      New_Stmt :=
        Build_Fat_Binary_Handle_Declaration
          (Fat_Binary_Handle, Fat_Binary_Wrapper);
      Append_To (Pack_Decls, New_Stmt);
      Analyze (New_Stmt);

      CUDA_Init_Func := Make_Temporary (Loc, 'C');
      New_Stmt :=
        Build_CUDA_Init_Proc
          (Init_Id      => CUDA_Init_Func,
           CUDA_Kernels => CUDA_Node_List,
           Handle_Id    => Fat_Binary_Handle,
           Pack_Decls   => Pack_Decls);
      Append_To (Pack_Decls, New_Stmt);
      Analyze (New_Stmt);

      New_Stmt :=
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (CUDA_Init_Func, Loc));
      Append_To (Pack_Decls, New_Stmt);
      Analyze (New_Stmt);
   end Build_And_Insert_CUDA_Initialization;

   ---------------------------------
   -- Remove_CUDA_Device_Entities --
   ---------------------------------

   procedure Remove_CUDA_Device_Entities (Pack_Id : Entity_Id) is
      Device_Entities : constant Elist_Id :=
        Get_CUDA_Device_Entities (Pack_Id);
      Device_Elmt     : Elmt_Id;
      Device_Entity   : Entity_Id;
      Bod             : Node_Id;
   begin
      pragma Assert (Debug_Flag_Underscore_C);

      if Device_Entities = No_Elist then
         return;
      end if;

      Device_Elmt := First_Elmt (Device_Entities);
      while Present (Device_Elmt) loop
         Device_Entity := Node (Device_Elmt);
         Next_Elmt (Device_Elmt);

         case Ekind (Device_Entity) is
            when E_Function | E_Procedure =>
               Bod := Subprogram_Body (Device_Entity);

               if Nkind (Parent (Bod)) = N_Subunit
                 and then Present (Corresponding_Stub (Parent (Bod)))
               then
                  Error_Msg_N
                    ("Cuda_Device not suported on separate subprograms",
                     Corresponding_Stub (Parent (Bod)));
               else
                  Remove (Bod);
                  Remove (Subprogram_Spec (Device_Entity));
               end if;

            when E_Variable | E_Constant =>
               Remove (Declaration_Node (Device_Entity));

            when others =>
               pragma Assert (False);
         end case;

         Remove_Entity_And_Homonym (Device_Entity);
      end loop;
   end Remove_CUDA_Device_Entities;

   ------------------------------
   -- Set_CUDA_Device_Entities --
   ------------------------------

   procedure Set_CUDA_Device_Entities
     (Pack_Id : Entity_Id;
      E       : Elist_Id)
   is
   begin
      pragma Assert (Get_CUDA_Device_Entities (Pack_Id) = No_Elist);
      CUDA_Device_Entities_Table.Set (Pack_Id, E);
   end Set_CUDA_Device_Entities;

   ----------------------
   -- Set_CUDA_Kernels --
   ----------------------

   procedure Set_CUDA_Kernels
     (Pack_Id : Entity_Id;
      Kernels : Elist_Id)
   is
   begin
      pragma Assert (Get_CUDA_Kernels (Pack_Id) = No_Elist);
      CUDA_Kernels_Table.Set (Pack_Id, Kernels);
   end Set_CUDA_Kernels;

end GNAT_CUDA;
