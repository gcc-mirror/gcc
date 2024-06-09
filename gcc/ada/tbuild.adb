------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T B U I L D                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Aspects;        use Aspects;
with Csets;          use Csets;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Lib;            use Lib;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Sinfo.Utils;    use Sinfo.Utils;
with Sem_Util;       use Sem_Util;
with Snames;         use Snames;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Urealp;         use Urealp;

package body Tbuild is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Unique_Serial_Number;
   --  Add a unique serialization to the string in the Name_Buffer. This
   --  consists of a unit specific serial number, and b/s for body/spec.

   ------------------------------
   -- Add_Unique_Serial_Number --
   ------------------------------

   Config_Serial_Number : Nat := 0;
   --  Counter for use in config pragmas, see comment below

   procedure Add_Unique_Serial_Number is
   begin
      --  If we are analyzing configuration pragmas, Cunit (Main_Unit) will
      --  not be set yet. This happens for example when analyzing static
      --  string expressions in configuration pragmas. For this case, we
      --  just maintain a local counter, defined above and we do not need
      --  to add a b or s indication in this case.

      if No (Cunit (Current_Sem_Unit)) then
         Config_Serial_Number := Config_Serial_Number + 1;
         Add_Nat_To_Name_Buffer (Config_Serial_Number);
         return;

      --  Normal case, within a unit

      else
         declare
            Unit_Node : constant Node_Id := Unit (Cunit (Current_Sem_Unit));

         begin
            Add_Nat_To_Name_Buffer (Increment_Serial_Number);

            --  Add either b or s, depending on whether current unit is a spec
            --  or a body. This is needed because we may generate the same name
            --  in a spec and a body otherwise.

            Name_Len := Name_Len + 1;

            if Nkind (Unit_Node) = N_Package_Declaration
              or else Nkind (Unit_Node) = N_Subprogram_Declaration
              or else Nkind (Unit_Node) in N_Generic_Declaration
            then
               Name_Buffer (Name_Len) := 's';
            else
               Name_Buffer (Name_Len) := 'b';
            end if;
         end;
      end if;
   end Add_Unique_Serial_Number;

   ----------------
   -- Checks_Off --
   ----------------

   function Checks_Off (N : Node_Id) return Node_Id is
   begin
      return
        Make_Unchecked_Expression (Sloc (N),
          Expression => N);
   end Checks_Off;

   ----------------
   -- Convert_To --
   ----------------

   function Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id is
      pragma Assert (Is_Type (Typ));
      Result : Node_Id;

   begin
      if Present (Etype (Expr)) and then Etype (Expr) = Typ then
         return Relocate_Node (Expr);

      else
         Result :=
           Make_Type_Conversion (Sloc (Expr),
             Subtype_Mark => New_Occurrence_Of (Typ, Sloc (Expr)),
             Expression => Relocate_Node (Expr));

         Set_Etype (Result, Typ);
         return Result;
      end if;
   end Convert_To;

   ----------------------------
   -- Convert_To_And_Rewrite --
   ----------------------------

   procedure Convert_To_And_Rewrite (Typ : Entity_Id; Expr : Node_Id) is
   begin
      Rewrite (Expr, Convert_To (Typ, Expr));
   end Convert_To_And_Rewrite;

   ------------------
   -- Discard_List --
   ------------------

   procedure Discard_List (L : List_Id) is
      pragma Warnings (Off, L);
   begin
      null;
   end Discard_List;

   ------------------
   -- Discard_Node --
   ------------------

   procedure Discard_Node (N : Node_Or_Entity_Id) is
      pragma Warnings (Off, N);
   begin
      null;
   end Discard_Node;

   -------------------------------------------
   -- Make_Byte_Aligned_Attribute_Reference --
   -------------------------------------------

   function Make_Byte_Aligned_Attribute_Reference
     (Sloc           : Source_Ptr;
      Prefix         : Node_Id;
      Attribute_Name : Name_Id)
      return           Node_Id
   is
      N : constant Node_Id :=
            Make_Attribute_Reference (Sloc,
              Prefix        => Prefix,
              Attribute_Name => Attribute_Name);

   begin
      pragma Assert
        (Attribute_Name in Name_Address | Name_Unrestricted_Access);
      Set_Must_Be_Byte_Aligned (N, True);
      return N;
   end Make_Byte_Aligned_Attribute_Reference;

   ------------------------
   -- Make_Float_Literal --
   ------------------------

   function Make_Float_Literal
     (Loc         : Source_Ptr;
      Radix       : Uint;
      Significand : Uint;
      Exponent    : Uint) return Node_Id
   is
   begin
      if Radix = 2 and then abs Significand /= 1 then
         return
           Make_Float_Literal
             (Loc, Uint_16,
              Significand * Radix**(Exponent mod 4),
              Exponent / 4);

      else
         declare
            N : constant Node_Id := New_Node (N_Real_Literal, Loc);

         begin
            Set_Realval (N,
              UR_From_Components
                (Num      => abs Significand,
                 Den      => -Exponent,
                 Rbase    => UI_To_Int (Radix),
                 Negative => Significand < 0));
            return N;
         end;
      end if;
   end Make_Float_Literal;

   -------------
   -- Make_Id --
   -------------

   function Make_Id (Str : Text_Buffer) return Node_Id is
   begin
      Name_Len := 0;

      for J in Str'Range loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Fold_Lower (Str (J));
      end loop;

      return
        Make_Identifier (System_Location,
          Chars => Name_Find);
   end Make_Id;

   -------------------------------------
   -- Make_Implicit_Exception_Handler --
   -------------------------------------

   function Make_Implicit_Exception_Handler
     (Sloc              : Source_Ptr;
      Choice_Parameter  : Node_Id := Empty;
      Exception_Choices : List_Id;
      Statements        : List_Id) return Node_Id
   is
      Handler : Node_Id;
      Loc     : Source_Ptr;

   begin
      --  Set the source location only when debugging the expanded code

      --  When debugging the source code directly, we do not want the compiler
      --  to associate this implicit exception handler with any specific source
      --  line, because it can potentially confuse the debugger. The most
      --  damaging situation would arise when the debugger tries to insert a
      --  breakpoint at a certain line. If the code of the associated implicit
      --  exception handler is generated before the code of that line, then the
      --  debugger will end up inserting the breakpoint inside the exception
      --  handler, rather than the code the user intended to break on. As a
      --  result, it is likely that the program will not hit the breakpoint
      --  as expected.

      if Debug_Generated_Code then
         Loc := Sloc;
      else
         Loc := No_Location;
      end if;

      Handler :=
        Make_Exception_Handler
          (Loc, Choice_Parameter, Exception_Choices, Statements);
      Set_Local_Raise_Statements (Handler, No_Elist);
      return Handler;
   end Make_Implicit_Exception_Handler;

   --------------------------------
   -- Make_Implicit_If_Statement --
   --------------------------------

   function Make_Implicit_If_Statement
     (Node            : Node_Id;
      Condition       : Node_Id;
      Then_Statements : List_Id;
      Elsif_Parts     : List_Id := No_List;
      Else_Statements : List_Id := No_List) return Node_Id
   is
   begin
      Check_Restriction (No_Implicit_Conditionals, Node);

      return Make_If_Statement (Sloc (Node),
        Condition,
        Then_Statements,
        Elsif_Parts,
        Else_Statements);
   end Make_Implicit_If_Statement;

   -------------------------------------
   -- Make_Implicit_Label_Declaration --
   -------------------------------------

   function Make_Implicit_Label_Declaration
     (Loc                 : Source_Ptr;
      Defining_Identifier : Node_Id;
      Label_Construct     : Node_Id) return Node_Id
   is
      N : constant Node_Id :=
            Make_Implicit_Label_Declaration (Loc, Defining_Identifier);
   begin
      Set_Label_Construct (N, Label_Construct);
      return N;
   end Make_Implicit_Label_Declaration;

   ----------------------------------
   -- Make_Implicit_Loop_Statement --
   ----------------------------------

   function Make_Implicit_Loop_Statement
     (Node                   : Node_Id;
      Statements             : List_Id;
      Identifier             : Node_Id := Empty;
      Iteration_Scheme       : Node_Id := Empty;
      Has_Created_Identifier : Boolean := False;
      End_Label              : Node_Id := Empty) return Node_Id
   is
      P                  : Node_Id;
      Check_Restrictions : Boolean := True;
   begin
      --  Do not check restrictions if the implicit loop statement is part
      --  of a dead branch: False and then ...
      --  This will occur in particular as part of the expansion of pragma
      --  Assert when assertions are disabled.

      P := Parent (Node);
      while Present (P) loop
         if Nkind (P) = N_And_Then then
            if Nkind (Left_Opnd (P)) = N_Identifier
              and then Entity (Left_Opnd (P)) = Standard_False
            then
               Check_Restrictions := False;
               exit;
            end if;

         --  Prevent the search from going too far

         elsif Is_Body_Or_Package_Declaration (P) then
            exit;
         end if;

         P := Parent (P);
      end loop;

      if Check_Restrictions then
         Check_Restriction (No_Implicit_Loops, Node);

         if Present (Iteration_Scheme)
           and then Nkind (Iteration_Scheme) /= N_Iterator_Specification
           and then Present (Condition (Iteration_Scheme))
         then
            Check_Restriction (No_Implicit_Conditionals, Node);
         end if;
      end if;

      return Make_Loop_Statement (Sloc (Node),
        Identifier             => Identifier,
        Iteration_Scheme       => Iteration_Scheme,
        Statements             => Statements,
        Has_Created_Identifier => Has_Created_Identifier,
        End_Label              => End_Label);
   end Make_Implicit_Loop_Statement;

   --------------------
   -- Make_Increment --
   --------------------

   function Make_Increment
     (Loc : Source_Ptr; Index : Entity_Id; Typ : Entity_Id) return Node_Id is
   begin
      return Make_Assignment_Statement (Loc,
               Name => New_Occurrence_Of (Index, Loc),
               Expression =>
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     New_Occurrence_Of (Typ, Loc),
                   Attribute_Name => Name_Succ,
                   Expressions => New_List (
                     New_Occurrence_Of (Index, Loc))));
   end Make_Increment;

   --------------------------
   -- Make_Integer_Literal --
   ---------------------------

   function Make_Integer_Literal
     (Loc    : Source_Ptr;
      Intval : Int) return Node_Id
   is
   begin
      return Make_Integer_Literal (Loc, UI_From_Int (Intval));
   end Make_Integer_Literal;

   --------------------------------
   -- Make_Linker_Section_Pragma --
   --------------------------------

   function Make_Linker_Section_Pragma
     (Ent : Entity_Id;
      Loc : Source_Ptr;
      Sec : String) return Node_Id
   is
      LS : Node_Id;

   begin
      LS :=
        Make_Pragma
          (Loc,
           Name_Linker_Section,
           New_List
             (Make_Pragma_Argument_Association
                (Sloc => Loc,
                 Expression => New_Occurrence_Of (Ent, Loc)),
              Make_Pragma_Argument_Association
                (Sloc => Loc,
                 Expression =>
                   Make_String_Literal
                     (Sloc => Loc,
                      Strval => Sec))));

      Set_Has_Gigi_Rep_Item (Ent);
      return LS;
   end Make_Linker_Section_Pragma;

   -----------------
   -- Make_Pragma --
   -----------------

   function Make_Pragma
     (Sloc                         : Source_Ptr;
      Chars                        : Name_Id;
      Pragma_Argument_Associations : List_Id := No_List) return Node_Id
   is
   begin
      return
        Make_Pragma (Sloc,
          Pragma_Argument_Associations => Pragma_Argument_Associations,
          Pragma_Identifier            => Make_Identifier (Sloc, Chars));
   end Make_Pragma;

   ---------------------------------
   -- Make_Raise_Constraint_Error --
   ---------------------------------

   function Make_Raise_Constraint_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code) return Node_Id
   is
   begin
      pragma Assert (Rkind (Reason) = CE_Reason);
      return
        Make_Raise_Constraint_Error (Sloc,
          Condition => Condition,
          Reason    => UI_From_Int (RT_Exception_Code'Pos (Reason)));
   end Make_Raise_Constraint_Error;

   ------------------------------
   -- Make_Raise_Program_Error --
   ------------------------------

   function Make_Raise_Program_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code) return Node_Id
   is
   begin
      pragma Assert (Rkind (Reason) = PE_Reason);
      return
        Make_Raise_Program_Error (Sloc,
          Condition => Condition,
          Reason    => UI_From_Int (RT_Exception_Code'Pos (Reason)));
   end Make_Raise_Program_Error;

   ------------------------------
   -- Make_Raise_Storage_Error --
   ------------------------------

   function Make_Raise_Storage_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code) return Node_Id
   is
   begin
      pragma Assert (Rkind (Reason) = SE_Reason);
      return
        Make_Raise_Storage_Error (Sloc,
          Condition => Condition,
          Reason    => UI_From_Int (RT_Exception_Code'Pos (Reason)));
   end Make_Raise_Storage_Error;

   -------------
   -- Make_SC --
   -------------

   function  Make_SC (Pre, Sel : Node_Id) return Node_Id is
   begin
      return
        Make_Selected_Component (System_Location,
          Prefix        => Pre,
          Selector_Name => Sel);
   end Make_SC;

   -------------------------
   -- Make_String_Literal --
   -------------------------

   function Make_String_Literal
     (Sloc   : Source_Ptr;
      Strval : String) return Node_Id
   is
   begin
      Start_String;
      Store_String_Chars (Strval);
      return Make_String_Literal (Sloc, Strval => End_String);
   end Make_String_Literal;

   -------------------------
   -- Make_Suppress_Block --
   -------------------------

   --  Generates the following expansion:

   --    declare
   --       pragma Suppress (<check>);
   --    begin
   --       <stmts>
   --    end;

   function Make_Suppress_Block
     (Loc   : Source_Ptr;
      Check : Name_Id;
      Stmts : List_Id) return Node_Id
   is
   begin
      return
        Make_Block_Statement (Loc,
          Declarations => New_List (
            Make_Pragma (Loc,
              Chars => Name_Suppress,
              Pragma_Argument_Associations => New_List (
                Make_Pragma_Argument_Association (Loc,
                  Expression => Make_Identifier (Loc, Check))))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));
   end Make_Suppress_Block;

   --------------------
   -- Make_Temporary --
   --------------------

   function Make_Temporary
     (Loc          : Source_Ptr;
      Id           : Character;
      Related_Node : Node_Id := Empty) return Entity_Id
   is
      Temp : constant Entity_Id :=
               Make_Defining_Identifier (Loc, Chars => New_Internal_Name (Id));
   begin
      Set_Related_Expression (Temp, Related_Node);
      return Temp;
   end Make_Temporary;

   ---------------------------
   -- Make_Unsuppress_Block --
   ---------------------------

   --  Generates the following expansion:

   --    declare
   --       pragma Unsuppress (<check>);
   --    begin
   --       <stmts>
   --    end;

   function Make_Unsuppress_Block
     (Loc   : Source_Ptr;
      Check : Name_Id;
      Stmts : List_Id) return Node_Id
   is
   begin
      return
        Make_Block_Statement (Loc,
          Declarations => New_List (
            Make_Pragma (Loc,
              Chars => Name_Unsuppress,
              Pragma_Argument_Associations => New_List (
                Make_Pragma_Argument_Association (Loc,
                  Expression => Make_Identifier (Loc, Check))))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));
   end Make_Unsuppress_Block;

   --------------------------
   -- New_Constraint_Error --
   --------------------------

   function New_Constraint_Error (Loc : Source_Ptr) return Node_Id is
      Ident_Node : Node_Id;
      Raise_Node : Node_Id;

   begin
      Ident_Node := New_Node (N_Identifier, Loc);
      Set_Chars (Ident_Node, Chars (Standard_Entity (S_Constraint_Error)));
      Set_Entity (Ident_Node, Standard_Entity (S_Constraint_Error));
      Raise_Node := New_Node (N_Raise_Statement, Loc);
      Set_Name (Raise_Node, Ident_Node);
      return Raise_Node;
   end New_Constraint_Error;

   -----------------------
   -- New_External_Name --
   -----------------------

   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : Character := ' ';
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ') return Name_Id
   is
   begin
      Get_Name_String (Related_Id);

      if Prefix /= ' ' then
         pragma Assert (Is_OK_Internal_Letter (Prefix) or else Prefix = '_');

         for J in reverse 1 .. Name_Len loop
            Name_Buffer (J + 1) := Name_Buffer (J);
         end loop;

         Name_Len := Name_Len + 1;
         Name_Buffer (1) := Prefix;
      end if;

      if Suffix /= ' ' then
         pragma Assert (Is_OK_Internal_Letter (Suffix));
         Add_Char_To_Name_Buffer (Suffix);
      end if;

      if Suffix_Index /= 0 then
         if Suffix_Index < 0 then
            Add_Unique_Serial_Number;
         else
            Add_Nat_To_Name_Buffer (Suffix_Index);
         end if;
      end if;

      return Name_Find;
   end New_External_Name;

   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : String;
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ') return Name_Id
   is
   begin
      Get_Name_String (Related_Id);

      if Prefix /= ' ' then
         pragma Assert (Is_OK_Internal_Letter (Prefix));

         for J in reverse 1 .. Name_Len loop
            Name_Buffer (J + 1) := Name_Buffer (J);
         end loop;

         Name_Len := Name_Len + 1;
         Name_Buffer (1) := Prefix;
      end if;

      if Suffix /= "" then
         Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
         Name_Len := Name_Len + Suffix'Length;
      end if;

      if Suffix_Index /= 0 then
         if Suffix_Index < 0 then
            Add_Unique_Serial_Number;
         else
            Add_Nat_To_Name_Buffer (Suffix_Index);
         end if;
      end if;

      return Name_Find;
   end New_External_Name;

   function New_External_Name
     (Suffix       : Character;
      Suffix_Index : Nat) return Name_Id
   is
   begin
      Name_Buffer (1) := Suffix;
      Name_Len := 1;
      Add_Nat_To_Name_Buffer (Suffix_Index);
      return Name_Find;
   end New_External_Name;

   -----------------------
   -- New_Internal_Name --
   -----------------------

   function New_Internal_Name (Id_Char : Character) return Name_Id is
   begin
      pragma Assert (Is_OK_Internal_Letter (Id_Char));
      Name_Buffer (1) := Id_Char;
      Name_Len := 1;
      Add_Unique_Serial_Number;
      return Name_Enter;
   end New_Internal_Name;

   -----------------------
   -- New_Occurrence_Of --
   -----------------------

   function New_Occurrence_Of
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr) return Node_Id
   is
      pragma Assert (Present (Def_Id) and then Nkind (Def_Id) in N_Entity);
      Occurrence : constant Node_Id :=
        Make_Identifier (Loc, Chars (Def_Id));

   begin
      Set_Entity (Occurrence, Def_Id);

      if Is_Type (Def_Id) then
         Set_Etype (Occurrence, Def_Id);
      else
         Set_Etype (Occurrence, Etype (Def_Id));
      end if;

      if Ekind (Def_Id) = E_Enumeration_Literal then
         Set_Is_Static_Expression (Occurrence, True);
      end if;

      return Occurrence;
   end New_Occurrence_Of;

   -----------------
   -- New_Op_Node --
   -----------------

   function New_Op_Node
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Node_Id
   is
      type Name_Of_Type is array (N_Op) of Name_Id;
      Name_Of : constant Name_Of_Type := Name_Of_Type'(
         N_Op_And                    => Name_Op_And,
         N_Op_Or                     => Name_Op_Or,
         N_Op_Xor                    => Name_Op_Xor,
         N_Op_Eq                     => Name_Op_Eq,
         N_Op_Ne                     => Name_Op_Ne,
         N_Op_Lt                     => Name_Op_Lt,
         N_Op_Le                     => Name_Op_Le,
         N_Op_Gt                     => Name_Op_Gt,
         N_Op_Ge                     => Name_Op_Ge,
         N_Op_Add                    => Name_Op_Add,
         N_Op_Subtract               => Name_Op_Subtract,
         N_Op_Concat                 => Name_Op_Concat,
         N_Op_Multiply               => Name_Op_Multiply,
         N_Op_Divide                 => Name_Op_Divide,
         N_Op_Mod                    => Name_Op_Mod,
         N_Op_Rem                    => Name_Op_Rem,
         N_Op_Expon                  => Name_Op_Expon,
         N_Op_Plus                   => Name_Op_Add,
         N_Op_Minus                  => Name_Op_Subtract,
         N_Op_Abs                    => Name_Op_Abs,
         N_Op_Not                    => Name_Op_Not,

         --  We don't really need these shift operators, since they never
         --  appear as operators in the source, but the path of least
         --  resistance is to put them in (the aggregate must be complete).

         N_Op_Rotate_Left            => Name_Rotate_Left,
         N_Op_Rotate_Right           => Name_Rotate_Right,
         N_Op_Shift_Left             => Name_Shift_Left,
         N_Op_Shift_Right            => Name_Shift_Right,
         N_Op_Shift_Right_Arithmetic => Name_Shift_Right_Arithmetic);

      Nod : constant Node_Id := New_Node (New_Node_Kind, New_Sloc);

   begin
      if New_Node_Kind in Name_Of'Range then
         Set_Chars (Nod, Name_Of (New_Node_Kind));
      end if;

      return Nod;
   end New_Op_Node;

   -----------------------
   -- New_Suffixed_Name --
   -----------------------

   function New_Suffixed_Name
     (Related_Id : Name_Id;
      Suffix     : String) return Name_Id
   is
   begin
      Get_Name_String (Related_Id);
      Add_Char_To_Name_Buffer ('_');
      Add_Str_To_Name_Buffer (Suffix);
      return Name_Find;
   end New_Suffixed_Name;

   -------------------
   -- OK_Convert_To --
   -------------------

   function OK_Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id is
      Result : Node_Id;
   begin
      Result :=
        Make_Type_Conversion (Sloc (Expr),
          Subtype_Mark => New_Occurrence_Of (Typ, Sloc (Expr)),
          Expression   => Relocate_Node (Expr));
      Set_Conversion_OK (Result, True);
      Set_Etype (Result, Typ);
      return Result;
   end OK_Convert_To;

   --------------
   -- Sel_Comp --
   --------------

   function Sel_Comp (Pre : Node_Id; Sel : String) return Node_Id is
   begin
      return Make_Selected_Component
        (Sloc          => Sloc (Pre),
         Prefix        => Pre,
         Selector_Name => Make_Identifier (Sloc (Pre), Name_Find (Sel)));
   end Sel_Comp;

   function Sel_Comp (Pre, Sel : String; Loc : Source_Ptr) return Node_Id is
   begin
      return Sel_Comp (Make_Identifier (Loc, Name_Find (Pre)), Sel);
   end Sel_Comp;

   -------------
   -- Set_NOD --
   -------------

   procedure Set_NOD (Unit : Node_Id) is
   begin
      Set_Restriction_No_Dependence (Unit, Warn => False);
   end Set_NOD;

   -------------
   -- Set_NSA --
   -------------

   procedure Set_NSA (Asp : Name_Id; OK : out Boolean) is
      Asp_Id : constant Aspect_Id := Get_Aspect_Id (Asp);
   begin
      if Asp_Id = No_Aspect then
         OK := False;
      else
         OK := True;
         Set_Restriction_No_Specification_Of_Aspect (Asp_Id);
      end if;
   end Set_NSA;

   -------------
   -- Set_NUA --
   -------------

   procedure Set_NUA (Attr : Name_Id; OK : out Boolean) is
   begin
      if Is_Attribute_Name (Attr) then
         OK := True;
         Set_Restriction_No_Use_Of_Attribute (Get_Attribute_Id (Attr));
      else
         OK := False;
      end if;
   end Set_NUA;

   -------------
   -- Set_NUP --
   -------------

   procedure Set_NUP (Prag : Name_Id; OK : out Boolean) is
   begin
      if Is_Pragma_Name (Prag) then
         OK := True;
         Set_Restriction_No_Use_Of_Pragma (Get_Pragma_Id (Prag));
      else
         OK := False;
      end if;
   end Set_NUP;

   --------------------------
   -- Unchecked_Convert_To --
   --------------------------

   function Unchecked_Convert_To
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id
   is
      pragma Assert (Ekind (Typ) in E_Void | Type_Kind);
      --  We don't really want to allow E_Void here, but existing code passes
      --  it.

      Loc    : constant Source_Ptr := Sloc (Expr);
      Result : Node_Id;

   begin
      --  If the expression is already of the correct type, then nothing
      --  to do, except for relocating the node

      if Present (Etype (Expr))
        and then (Base_Type (Etype (Expr)) = Typ or else Etype (Expr) = Typ)
      then
         return Relocate_Node (Expr);

      --  Case where the expression is already an unchecked conversion. We
      --  replace the type being converted to, to avoid creating an unchecked
      --  conversion of an unchecked conversion. Extra unchecked conversions
      --  make the .dg output less readable. We can't do this in cases
      --  involving bitfields, because the sizes might not match. The
      --  Is_Composite_Type checks avoid such cases.

      elsif Nkind (Expr) = N_Unchecked_Type_Conversion
        and then Is_Composite_Type (Etype (Expr))
        and then Is_Composite_Type (Typ)
      then
         Set_Subtype_Mark (Expr, New_Occurrence_Of (Typ, Loc));
         Result := Relocate_Node (Expr);

      elsif Nkind (Expr) = N_Null
        and then Is_Access_Type (Typ)
      then
         --  No need for a conversion

         Result := Relocate_Node (Expr);

      --  All other cases

      else
         declare
            Expr_Parent : constant Node_Id := Parent (Expr);
         begin
            Result :=
              Make_Unchecked_Type_Conversion (Loc,
                Subtype_Mark => New_Occurrence_Of (Typ, Loc),
                Expression   => Relocate_Node (Expr));
            Set_Parent (Result, Expr_Parent);
         end;
      end if;

      Set_Etype (Result, Typ);
      return Result;
   end Unchecked_Convert_To;

end Tbuild;
