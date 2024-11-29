------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ I M G V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2024, Free Software Foundation, Inc.         --
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
with Casing;         use Casing;
with Checks;         use Checks;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Debug;          use Debug;
with Exp_Put_Image;
with Exp_Util;       use Exp_Util;
with Lib;            use Lib;
with Namet;          use Namet;
with Nmake;          use Nmake;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem_Aux;        use Sem_Aux;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Targparm;       use Targparm;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Uintp;          use Uintp;
with Urealp;         use Urealp;

with System.Perfect_Hash_Generators;

package body Exp_Imgv is

   procedure Rewrite_Object_Image
     (N         : Node_Id;
      Pref      : Node_Id;
      Attr_Name : Name_Id;
      Str_Typ   : Entity_Id);
   --  AI12-0124: Rewrite attribute 'Image when it is applied to an object
   --  reference as an attribute applied to a type. N denotes the node to be
   --  rewritten, Pref denotes the prefix of the 'Image attribute, and Name
   --  and Str_Typ specify which specific string type and 'Image attribute to
   --  apply (e.g. Name_Wide_Image and Standard_Wide_String).

   ------------------------------------
   -- Build_Enumeration_Image_Tables --
   ------------------------------------

   procedure Build_Enumeration_Image_Tables (E : Entity_Id; N : Node_Id) is
      Loc          : constant Source_Ptr := Sloc (E);
      In_Main_Unit : constant Boolean    := In_Extended_Main_Code_Unit (Loc);

      Act  : List_Id;
      Eind : Entity_Id;
      Estr : Entity_Id;
      H_Id : Entity_Id;
      H_OK : Boolean;
      H_Sp : Node_Id;
      Ind  : List_Id;
      Ityp : Node_Id;
      Len  : Nat;
      Lit  : Entity_Id;
      Nlit : Nat;
      S_Id : Entity_Id;
      S_N  : Nat := 0;
      Str  : String_Id;

      package SPHG renames System.Perfect_Hash_Generators;

      Saved_SSO : constant Character := Opt.Default_SSO;
      --  Used to save the current scalar storage order during the generation
      --  of the literal lookup table.

      Serial_Number_Budget : constant := 50;
      --  We may want to compute a perfect hash function for use by the Value
      --  attribute. However computing this function is costly and, therefore,
      --  cannot be done when compiling every unit where the enumeration type
      --  is referenced, so we do it only when compiling the unit where it is
      --  declared. This means that we may need to control the internal serial
      --  numbers of this unit, or else we would risk generating public symbols
      --  with mismatched names later on. The strategy for this is to allocate
      --  a fixed budget of serial numbers to be spent from a specified point
      --  until the end of the processing and to make sure that it is always
      --  exactly spent on all possible paths from this point.

      Threshold : constant Nat :=
        (if Is_Library_Level_Entity (E)
           or else not Always_Compatible_Rep_On_Target
         then 3
         else Nat'Last);
      --  Threshold above which we want to generate the hash function in the
      --  default case. We avoid doing it if this would cause a trampoline to
      --  be generated because the type is local and descriptors are not used.

      Threshold_For_Size : constant Nat := Nat'Max (Threshold, 9);
      --  But the function and its tables take a bit of space so the threshold
      --  is raised when compiling for size.

      procedure Append_Table_To
        (L    : List_Id;
         E    : Entity_Id;
         UB   : Nat;
         Ctyp : Entity_Id;
         V    : List_Id);
      --  Append to L the declaration of E as a constant array of range 0 .. UB
      --  and component type Ctyp with initial value V.

      ---------------------
      -- Append_Table_To --
      ---------------------

      procedure Append_Table_To
        (L    : List_Id;
         E    : Entity_Id;
         UB   : Nat;
         Ctyp : Entity_Id;
         V    : List_Id)
      is
      begin
         Append_To (L,
           Make_Object_Declaration (Loc,
             Defining_Identifier => E,
             Constant_Present    => True,
             Object_Definition   =>
               Make_Constrained_Array_Definition (Loc,
                 Discrete_Subtype_Definitions => New_List (
                   Make_Range (Loc,
                     Low_Bound  => Make_Integer_Literal (Loc, 0),
                     High_Bound => Make_Integer_Literal (Loc, UB))),
                 Component_Definition =>
                   Make_Component_Definition (Loc,
                     Aliased_Present    => False,
                     Subtype_Indication => New_Occurrence_Of (Ctyp, Loc))),
             Expression          => Make_Aggregate (Loc, Expressions => V)));
      end Append_Table_To;

   --  Start of Build_Enumeration_Image_Tables

   begin
      --  Nothing to do for types other than a root enumeration type

      if E /= Root_Type (E) then
         return;

      --  Nothing to do if pragma Discard_Names applies

      elsif Discard_Names (E) then
         return;
      end if;

      --  Otherwise tables need constructing

      Start_String;
      Ind := New_List;
      Lit := First_Literal (E);
      Len := 1;
      Nlit := 0;
      H_OK := False;

      loop
         Append_To (Ind, Make_Integer_Literal (Loc, UI_From_Int (Len)));

         exit when No (Lit);
         Nlit := Nlit + 1;

         Get_Unqualified_Decoded_Name_String (Chars (Lit));

         if Name_Buffer (1) /= ''' then
            Set_Casing (All_Upper_Case);
         end if;

         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         if In_Main_Unit then
            SPHG.Insert (Name_Buffer (1 .. Name_Len));
         end if;
         Len := Len + Int (Name_Len);
         Next_Literal (Lit);
      end loop;

      if Len < Int (2 ** (8 - 1)) then
         Ityp := Standard_Integer_8;
      elsif Len < Int (2 ** (16 - 1)) then
         Ityp := Standard_Integer_16;
      else
         Ityp := Standard_Integer_32;
      end if;

      Str := End_String;

      Estr :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (E), 'S'));

      Eind :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (E), 'N'));

      Set_Lit_Strings (E, Estr);
      Set_Lit_Indexes (E, Eind);

      --  Temporarily set the current scalar storage order to the default
      --  during the generation of the literals table, since both the Image and
      --  Value attributes rely on runtime routines for interpreting table
      --  values.

      Opt.Default_SSO := ' ';

      --  Generate literal table

      Act :=
        New_List (
          Make_Object_Declaration (Loc,
            Defining_Identifier => Estr,
            Constant_Present    => True,
            Object_Definition   =>
              New_Occurrence_Of (Standard_String, Loc),
            Expression          =>
              Make_String_Literal (Loc,
                Strval => Str)));

      --  Generate index table

      Append_Table_To (Act, Eind, Nlit, Ityp, Ind);

      --  If the number of literals is not greater than Threshold, then we are
      --  done. Otherwise we generate a (perfect) hash function for use by the
      --  Value attribute.

      if Nlit > Threshold then
         --  We start to count serial numbers from here

         S_N := Increment_Serial_Number;

         --  Generate specification of hash function

         H_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (E), 'H'));
         Mutate_Ekind    (H_Id, E_Function);
         Set_Is_Internal (H_Id);

         if not Debug_Generated_Code then
            Set_Debug_Info_Off (H_Id);
         end if;

         Set_Lit_Hash (E, H_Id);

         S_Id := Make_Temporary (Loc, 'S');

         H_Sp := Make_Function_Specification (Loc,
           Defining_Unit_Name       => H_Id,
           Parameter_Specifications => New_List (
             Make_Parameter_Specification (Loc,
               Defining_Identifier => S_Id,
               Parameter_Type      =>
                 New_Occurrence_Of (Standard_String, Loc))),
           Result_Definition       =>
             New_Occurrence_Of (Standard_Natural, Loc));

         --  If the unit where the type is declared is the main unit, and the
         --  number of literals is greater than Threshold_For_Size when we are
         --  optimizing for size, and the restriction No_Implicit_Loops is not
         --  active, and -gnatd_h is not specified, and not GNAT_Mode, generate
         --  the hash function.

         if In_Main_Unit
           and then (Optimize_Size = 0 or else Nlit > Threshold_For_Size)
           and then not Restriction_Active (No_Implicit_Loops)
           and then not Debug_Flag_Underscore_H
           and then not GNAT_Mode
         then
            declare
               LB : constant Positive := 2 * Positive (Nlit) + 1;
               UB : constant Positive := LB + 24;

            begin
               --  Try at most 25 * 4 times to compute the hash function before
               --  giving up and using a linear search for the Value attribute.

               for V in LB .. UB loop
                  begin
                     SPHG.Initialize (4321, V, SPHG.Memory_Space, Tries => 4);
                     SPHG.Compute ("");
                     H_OK := True;
                     exit;
                  exception
                     when SPHG.Too_Many_Tries => null;
                  end;
               end loop;
            end;
         end if;

         --  If the hash function has been successfully computed, 4 more tables
         --  named P, T1, T2 and G are needed. The hash function is of the form

         --     function Hash (S : String) return Natural is
         --        xxxP  : constant array (0 .. X) of Natural    = [...];
         --        xxxT1 : constant array (0 .. Y) of Index_Type = [...];
         --        xxxT2 : constant array (0 .. Y) of Index_Type = [...];
         --        xxxG  : constant array (0 .. Z) of Index_Type = [...];

         --        F    : constant Natural := S'First - 1;
         --        L    : constant Natural := S'Length;
         --        A, B : Natural := 0;
         --        J    : Natural;

         --     begin
         --        for K in P'Range loop
         --           exit when L < P (K);
         --           J := Character'Pos (S (P (K) + F));
         --           A := (A + Natural (T1 (K) * J)) mod N;
         --           B := (B + Natural (T2 (K) * J)) mod N;
         --        end loop;

         --        return (Natural (G (A)) + Natural (G (B))) mod M;
         --     end Hash;

         --  where N is the length of G and M the number of literals. Note that
         --  we declare the tables inside the function for two reasons: first,
         --  their analysis creates array subtypes and thus their concatenation
         --  operators which are homonyms of the concatenation operator and may
         --  change the homonym number of user operators declared in the scope;
         --  second, the code generator can fold the values in the tables when
         --  they are small and avoid emitting them in the final object code.

         if H_OK then
            declare
               Siz, L1, L2 : Natural;
               I           : Int;

               Pos,  T1,  T2,  G  : List_Id;
               EPos, ET1, ET2, EG : Entity_Id;

               F, L, A, B, J, K : Entity_Id;
               Body_Decls       : List_Id;
               Body_Stmts       : List_Id;
               Loop_Stmts       : List_Id;

            begin
               Body_Decls := New_List;

               --  Generate position table

               SPHG.Define (SPHG.Character_Position, Siz, L1, L2);
               Pos := New_List;
               for J in 0 .. L1 - 1 loop
                  I := Int (SPHG.Value (SPHG.Character_Position, J));
                  Append_To (Pos, Make_Integer_Literal (Loc, UI_From_Int (I)));
               end loop;

               EPos :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (E), 'P'));

               Append_Table_To
                 (Body_Decls, EPos, Nat (L1 - 1), Standard_Natural, Pos);

               --  Generate function table 1

               SPHG.Define (SPHG.Function_Table_1, Siz, L1, L2);
               T1 := New_List;
               for J in 0 .. L1 - 1 loop
                  I := Int (SPHG.Value (SPHG.Function_Table_1, J));
                  Append_To (T1, Make_Integer_Literal (Loc, UI_From_Int (I)));
               end loop;

               ET1 :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (E), "T1"));

               Ityp :=
                 Small_Integer_Type_For (UI_From_Int (Int (Siz)), Uns => True);
               Append_Table_To (Body_Decls, ET1, Nat (L1 - 1), Ityp, T1);

               --  Generate function table 2

               SPHG.Define (SPHG.Function_Table_2, Siz, L1, L2);
               T2 := New_List;
               for J in 0 .. L1 - 1 loop
                  I := Int (SPHG.Value (SPHG.Function_Table_2, J));
                  Append_To (T2, Make_Integer_Literal (Loc, UI_From_Int (I)));
               end loop;

               ET2 :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (E), "T2"));

               Ityp :=
                 Small_Integer_Type_For (UI_From_Int (Int (Siz)), Uns => True);
               Append_Table_To (Body_Decls, ET2, Nat (L1 - 1), Ityp, T2);

               --  Generate graph table

               SPHG.Define (SPHG.Graph_Table, Siz, L1, L2);
               G := New_List;
               for J in 0 .. L1 - 1 loop
                  I := Int (SPHG.Value (SPHG.Graph_Table, J));
                  Append_To (G, Make_Integer_Literal (Loc, UI_From_Int (I)));
               end loop;

               EG :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (E), 'G'));

               Ityp :=
                 Small_Integer_Type_For (UI_From_Int (Int (Siz)), Uns => True);
               Append_Table_To (Body_Decls, EG, Nat (L1 - 1), Ityp, G);

               F := Make_Temporary (Loc, 'F');

               Append_To (Body_Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => F,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Natural, Loc),
                   Expression          =>
                     Make_Op_Subtract (Loc,
                       Left_Opnd  =>
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Occurrence_Of (S_Id, Loc),
                           Attribute_Name => Name_First),
                       Right_Opnd =>
                         Make_Integer_Literal (Loc, 1))));

               L := Make_Temporary (Loc, 'L');

               Append_To (Body_Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => L,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Natural, Loc),
                   Expression          =>
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (S_Id, Loc),
                       Attribute_Name => Name_Length)));

               A := Make_Temporary (Loc, 'A');

               Append_To (Body_Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => A,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Natural, Loc),
                   Expression          => Make_Integer_Literal (Loc, 0)));

               B := Make_Temporary (Loc, 'B');

               Append_To (Body_Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => B,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Natural, Loc),
                   Expression          => Make_Integer_Literal (Loc, 0)));

               J := Make_Temporary (Loc, 'J');

               Append_To (Body_Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => J,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Natural, Loc)));

               K := Make_Temporary (Loc, 'K');

               --  Generate exit when L < P (K);

               Loop_Stmts := New_List (
                 Make_Exit_Statement (Loc,
                   Condition =>
                     Make_Op_Lt (Loc,
                       Left_Opnd  => New_Occurrence_Of (L, Loc),
                       Right_Opnd =>
                         Make_Indexed_Component (Loc,
                           Prefix      => New_Occurrence_Of (EPos, Loc),
                           Expressions => New_List (
                             New_Occurrence_Of (K, Loc))))));

               --  Generate J := Character'Pos (S (P (K) + F));

               Append_To (Loop_Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (J, Loc),
                   Expression =>
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         New_Occurrence_Of (Standard_Character, Loc),
                       Attribute_Name => Name_Pos,
                       Expressions    => New_List (
                         Make_Indexed_Component (Loc,
                           Prefix      => New_Occurrence_Of (S_Id, Loc),
                           Expressions => New_List (
                              Make_Op_Add (Loc,
                                Left_Opnd  =>
                                  Make_Indexed_Component (Loc,
                                    Prefix      =>
                                      New_Occurrence_Of (EPos, Loc),
                                  Expressions => New_List (
                                    New_Occurrence_Of (K, Loc))),
                                Right_Opnd =>
                                  New_Occurrence_Of (F, Loc))))))));

               --  Generate A := (A + Natural (T1 (K) * J)) mod N;

               Append_To (Loop_Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (A, Loc),
                   Expression =>
                     Make_Op_Mod (Loc,
                       Left_Opnd  =>
                          Make_Op_Add (Loc,
                            Left_Opnd  => New_Occurrence_Of (A, Loc),
                            Right_Opnd =>
                              Make_Op_Multiply (Loc,
                                Left_Opnd  =>
                                  Convert_To (Standard_Natural,
                                     Make_Indexed_Component (Loc,
                                       Prefix      =>
                                         New_Occurrence_Of (ET1, Loc),
                                       Expressions => New_List (
                                         New_Occurrence_Of (K, Loc)))),
                                Right_Opnd => New_Occurrence_Of (J, Loc))),
                       Right_Opnd => Make_Integer_Literal (Loc, Int (L1)))));

               --  Generate B := (B + Natural (T2 (K) * J)) mod N;

               Append_To (Loop_Stmts,
                 Make_Assignment_Statement (Loc,
                   Name       => New_Occurrence_Of (B, Loc),
                   Expression =>
                     Make_Op_Mod (Loc,
                       Left_Opnd  =>
                          Make_Op_Add (Loc,
                            Left_Opnd  => New_Occurrence_Of (B, Loc),
                            Right_Opnd =>
                              Make_Op_Multiply (Loc,
                                Left_Opnd  =>
                                  Convert_To (Standard_Natural,
                                     Make_Indexed_Component (Loc,
                                       Prefix      =>
                                         New_Occurrence_Of (ET2, Loc),
                                       Expressions => New_List (
                                         New_Occurrence_Of (K, Loc)))),
                                Right_Opnd => New_Occurrence_Of (J, Loc))),
                       Right_Opnd => Make_Integer_Literal (Loc, Int (L1)))));

               --  Generate loop

               Body_Stmts := New_List (
                 Make_Implicit_Loop_Statement (N,
                   Iteration_Scheme =>
                     Make_Iteration_Scheme (Loc,
                       Loop_Parameter_Specification =>
                         Make_Loop_Parameter_Specification (Loc,
                           Defining_Identifier         => K,
                           Discrete_Subtype_Definition =>
                             Make_Attribute_Reference (Loc,
                               Prefix         =>
                                 New_Occurrence_Of (EPos, Loc),
                               Attribute_Name => Name_Range))),
                   Statements       => Loop_Stmts));

               --  Generate return (Natural (G (A)) + Natural (G (B))) mod M;

               Append_To (Body_Stmts,
                 Make_Simple_Return_Statement (Loc,
                   Expression =>
                     Make_Op_Mod (Loc,
                       Left_Opnd  =>
                         Make_Op_Add (Loc,
                           Left_Opnd  =>
                             Convert_To (Standard_Natural,
                               Make_Indexed_Component (Loc,
                                 Prefix      =>
                                   New_Occurrence_Of (EG, Loc),
                                 Expressions => New_List (
                                   New_Occurrence_Of (A, Loc)))),
                           Right_Opnd =>
                             Convert_To (Standard_Natural,
                               Make_Indexed_Component (Loc,
                                 Prefix      =>
                                   New_Occurrence_Of (EG, Loc),
                                 Expressions => New_List (
                                   New_Occurrence_Of (B, Loc))))),
                       Right_Opnd => Make_Integer_Literal (Loc, Nlit))));

               --  Generate final body

               Append_To (Act,
                 Make_Subprogram_Body (Loc,
                   Specification => H_Sp,
                   Declarations => Body_Decls,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc, Body_Stmts)));
            end;

         --  If we chose not to or did not manage to compute the hash function,
         --  we need to build a dummy function always returning Natural'Last
         --  because other units reference it if they use the Value attribute.

         elsif In_Main_Unit then
            declare
               Body_Stmts : List_Id;

            begin
               --  Generate return Natural'Last

               Body_Stmts := New_List (
                 Make_Simple_Return_Statement (Loc,
                   Expression =>
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         New_Occurrence_Of (Standard_Natural, Loc),
                       Attribute_Name => Name_Last)));

               --  Generate body

               Append_To (Act,
                 Make_Subprogram_Body (Loc,
                   Specification => H_Sp,
                   Declarations => Empty_List,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc, Body_Stmts)));
            end;

         --  For the other units, just declare the function

         else
            Append_To (Act,
              Make_Subprogram_Declaration (Loc, Specification => H_Sp));
         end if;

      else
         Set_Lit_Hash (E, Empty);
      end if;

      if In_Main_Unit then
         System.Perfect_Hash_Generators.Finalize;
      end if;

      Insert_Actions (N, Act, Suppress => All_Checks);

      --  This is where we check that our budget of serial numbers has been
      --  entirely spent, see the declaration of Serial_Number_Budget above.

      if Nlit > Threshold then
         Synchronize_Serial_Number (S_N + Serial_Number_Budget);
      end if;

      --  Reset the scalar storage order to the saved value

      Opt.Default_SSO := Saved_SSO;
   end Build_Enumeration_Image_Tables;

   ----------------------------
   -- Expand_Image_Attribute --
   ----------------------------

   --  For all cases other than user-defined enumeration types, the scheme
   --  is as follows. First we insert the following code:

   --    Snn : String (1 .. rt'Width);
   --    Pnn : Natural;
   --    Image_xx (tv, Snn, Pnn [,pm]);
   --
   --  and then Expr is replaced by Snn (1 .. Pnn)

   --  In the above expansion:

   --    rt is the root type of the expression
   --    tv is the expression with the value, usually a type conversion
   --    pm is an extra parameter present in some cases

   --  The following table shows tv, xx, and (if used) pm for the various
   --  possible types of the argument:

   --    For types whose root type is Character
   --      xx = Character
   --      tv = Character (Expr)

   --    For types whose root type is Boolean
   --      xx = Boolean
   --      tv = Boolean (Expr)

   --    For signed integer types
   --      xx = [Long_Long_[Long_]]Integer
   --      tv = [Long_Long_[Long_]]Integer (Expr)

   --    For modular types
   --      xx = [Long_Long_[Long_]]Unsigned
   --      tv = System.Unsigned_Types.[Long_Long_[Long_]]Unsigned (Expr)

   --    For types whose root type is Wide_Character
   --      xx = Wide_Character
   --      tv = Wide_Character (Expr)
   --      pm = Boolean, true if Ada 2005 mode, False otherwise

   --    For types whose root type is Wide_Wide_Character
   --      xx = Wide_Wide_Character
   --      tv = Wide_Wide_Character (Expr)

   --    For floating-point types
   --      xx = Floating_Point
   --      tv = [Long_[Long_]]Float (Expr)
   --      pm = typ'Digits (typ = subtype of expression)

   --    For decimal fixed-point types
   --      xx = Decimal{32,64,128}
   --      tv = Integer_{32,64,128} (Expr)? [convert with no scaling]
   --      pm = typ'Scale (typ = subtype of expression)

   --    For the most common ordinary fixed-point types
   --      xx = Fixed{32,64,128}
   --      tv = Integer_{32,64,128} (Expr) [convert with no scaling]
   --      pm = numerator of typ'Small (typ = subtype of expression)
   --           denominator of typ'Small
   --           (Integer_{32,64,128} x typ'Small)'Fore
   --           typ'Aft

   --    For other ordinary fixed-point types
   --      xx = Fixed
   --      tv = Long_Float (Expr)
   --      pm = typ'Aft (typ = subtype of expression)

   --  For enumeration types other than those declared in package Standard
   --  or System, Snn, Pnn, are expanded as above, but the call looks like:

   --    Image_Enumeration_NN (rt'Pos (X), Snn, Pnn, typS, typI'Address)

   --  where rt is the root type of the expression, and typS and typI are
   --  the entities constructed as described in the spec for the procedure
   --  Build_Enumeration_Image_Tables and NN is 32/16/8 depending on the
   --  element type of Lit_Indexes. The rewriting of the expression to
   --  Snn (1 .. Pnn) then occurs as in the other cases. A special case is
   --  when pragma Discard_Names applies, in which case we replace expr by:

   --     (rt'Pos (expr))'Image

   --  So that the result is a space followed by the decimal value for the
   --  position of the enumeration value in the enumeration type.

   procedure Expand_Image_Attribute (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Exprs : constant List_Id    := Expressions (N);
      Expr  : constant Node_Id    := Relocate_Node (First (Exprs));
      Pref  : constant Node_Id    := Prefix (N);

      procedure Expand_Standard_Boolean_Image;
      --  Expand attribute 'Image in Standard.Boolean, avoiding string copy

      procedure Expand_User_Defined_Enumeration_Image (Typ : Entity_Id);
      --  Expand attribute 'Image in user-defined enumeration types, avoiding
      --  string copy.

      -----------------------------------
      -- Expand_Standard_Boolean_Image --
      -----------------------------------

      procedure Expand_Standard_Boolean_Image is
         Ins_List : constant List_Id   := New_List;
         S1_Id    : constant Entity_Id := Make_Temporary (Loc, 'S');
         T_Id     : constant Entity_Id := Make_Temporary (Loc, 'T');
         F_Id     : constant Entity_Id := Make_Temporary (Loc, 'F');
         V_Id     : constant Entity_Id := Make_Temporary (Loc, 'V');

      begin
         --  We use a single 5-character string subtype throughout so that the
         --  subtype of the string if-expression is constrained and, therefore,
         --  does not force the creation of a temporary during analysis.

         --  Generate:
         --    subtype S1 is String (1 .. 5);

         Append_To (Ins_List,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => S1_Id,
             Subtype_Indication  =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (Standard_String, Loc),
                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints => New_List (
                       Make_Range (Loc,
                         Low_Bound  => Make_Integer_Literal (Loc, 1),
                         High_Bound => Make_Integer_Literal (Loc, 5)))))));

         --  Generate:
         --    T : constant String (1 .. 5) := "TRUE ";

         Start_String;
         Store_String_Chars ("TRUE ");

         Append_To (Ins_List,
           Make_Object_Declaration (Loc,
             Defining_Identifier => T_Id,
             Object_Definition   =>
               New_Occurrence_Of (S1_Id, Loc),
             Constant_Present    => True,
             Expression          => Make_String_Literal (Loc, End_String)));

         --  Generate:
         --    F : constant String (1 .. 5) := "FALSE";

         Start_String;
         Store_String_Chars ("FALSE");

         Append_To (Ins_List,
           Make_Object_Declaration (Loc,
             Defining_Identifier => F_Id,
             Object_Definition   =>
               New_Occurrence_Of (S1_Id, Loc),
             Constant_Present    => True,
             Expression          => Make_String_Literal (Loc, End_String)));

         --  Generate:
         --    V : String (1 .. 5) renames (if Expr then T else F);

         Append_To (Ins_List,
           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => V_Id,
             Subtype_Mark        =>
               New_Occurrence_Of (S1_Id, Loc),
             Name                =>
               Make_If_Expression (Loc,
                 Expressions => New_List (
                   Duplicate_Subexpr (Expr),
                   New_Occurrence_Of (T_Id, Loc),
                   New_Occurrence_Of (F_Id, Loc)))));

         --  Insert all the above declarations before N. We suppress checks
         --  because everything is in range at this stage.

         Insert_Actions (N, Ins_List, Suppress => All_Checks);

         --  Final step is to rewrite the expression as a slice:
         --  V (1 .. (if Expr then 4 else 5)) and analyze, again with no
         --  checks, since we are sure that everything is OK.

         Rewrite (N,
           Make_Slice (Loc,
              Prefix         => New_Occurrence_Of (V_Id, Loc),
              Discrete_Range =>
                Make_Range (Loc,
                  Low_Bound  => Make_Integer_Literal (Loc, 1),
                  High_Bound =>
                    Make_If_Expression (Loc,
                      Expressions => New_List (
                        Duplicate_Subexpr (Expr),
                        Make_Integer_Literal (Loc, 4),
                        Make_Integer_Literal (Loc, 5))))));

         Analyze_And_Resolve (N, Standard_String, Suppress => All_Checks);
      end Expand_Standard_Boolean_Image;

      -------------------------------------------
      -- Expand_User_Defined_Enumeration_Image --
      -------------------------------------------

      procedure Expand_User_Defined_Enumeration_Image (Typ : Entity_Id) is
         Ins_List : constant List_Id   := New_List;
         P1_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         P2_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         P3_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         P4_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         S1_Id    : constant Entity_Id := Make_Temporary (Loc, 'S');

      begin
         --  Apply a validity check, since it is a bit drastic to get a
         --  completely junk image value for an invalid value.

         Insert_Valid_Check (Expr);

         --  Generate:
         --    P1 : constant Natural := Typ'Pos (Typ?(Expr));

         Append_To (Ins_List,
           Make_Object_Declaration (Loc,
             Defining_Identifier => P1_Id,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Natural, Loc),
             Constant_Present    => True,
             Expression          =>
               Convert_To (Standard_Natural,
                 Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Pos,
                   Prefix         => New_Occurrence_Of (Typ, Loc),
                   Expressions    => New_List (OK_Convert_To (Typ, Expr))))));

         --  Compute the index of the string start, generating:
         --    P2 : constant Natural := call_put_enumN (P1);

         Append_To (Ins_List,
           Make_Object_Declaration (Loc,
             Defining_Identifier => P2_Id,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Natural, Loc),
             Constant_Present    => True,
             Expression          =>
               Convert_To (Standard_Natural,
                 Make_Indexed_Component (Loc,
                   Prefix      =>
                     New_Occurrence_Of (Lit_Indexes (Typ), Loc),
                   Expressions =>
                     New_List (New_Occurrence_Of (P1_Id, Loc))))));

         --  Compute the index of the next value, generating:
         --    P3 : constant Natural := call_put_enumN (P1 + 1);

         declare
            Add_Node : constant Node_Id :=
              Make_Op_Add (Loc,
                Left_Opnd  => New_Occurrence_Of (P1_Id, Loc),
                Right_Opnd => Make_Integer_Literal (Loc, Uint_1));

         begin
            Append_To (Ins_List,
              Make_Object_Declaration (Loc,
                Defining_Identifier => P3_Id,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Natural, Loc),
                Constant_Present    => True,
                Expression          =>
                  Convert_To (Standard_Natural,
                    Make_Indexed_Component (Loc,
                      Prefix      =>
                        New_Occurrence_Of (Lit_Indexes (Typ), Loc),
                      Expressions =>
                        New_List (Add_Node)))));
         end;

         --  Generate:
         --    P4 : String renames call_put_enumS (P2 .. P3 - 1);

         declare
            Sub_Node : constant Node_Id :=
              Make_Op_Subtract (Loc,
                Left_Opnd  => New_Occurrence_Of (P3_Id, Loc),
                Right_Opnd => Make_Integer_Literal (Loc, Uint_1));

         begin
            Append_To (Ins_List,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => P4_Id,
                Subtype_Mark        =>
                  New_Occurrence_Of (Standard_String, Loc),
                Name                =>
                  Make_Slice (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Lit_Strings (Typ), Loc),
                    Discrete_Range =>
                      Make_Range (Loc,
                        Low_Bound  => New_Occurrence_Of (P2_Id, Loc),
                        High_Bound => Sub_Node))));
         end;

         --  Generate:
         --    subtype S1 is String (1 .. P3 - P2);

         declare
            HB : constant Node_Id :=
              Make_Op_Subtract (Loc,
                Left_Opnd  => New_Occurrence_Of (P3_Id, Loc),
                Right_Opnd => New_Occurrence_Of (P2_Id, Loc));

         begin
            Append_To (Ins_List,
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => S1_Id,
                Subtype_Indication  =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Occurrence_Of (Standard_String, Loc),
                    Constraint   =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints => New_List (
                          Make_Range (Loc,
                            Low_Bound  => Make_Integer_Literal (Loc, 1),
                            High_Bound => HB))))));
         end;

         --  Insert all the above declarations before N. We suppress checks
         --  because everything is in range at this stage.

         Insert_Actions (N, Ins_List, Suppress => All_Checks);

         Rewrite (N,
           Unchecked_Convert_To (S1_Id, New_Occurrence_Of (P4_Id, Loc)));

         Analyze_And_Resolve (N, Standard_String);
      end Expand_User_Defined_Enumeration_Image;

      --  Local variables

      Enum_Case : Boolean;
      Imid      : RE_Id;
      Proc_Ent  : Entity_Id;
      Ptyp      : Entity_Id;
      Rtyp      : Entity_Id;
      Tent      : Entity_Id := Empty;
      Ttyp      : Entity_Id;

      Arg_List : List_Id;
      --  List of arguments for run-time procedure call

      Ins_List : List_Id;
      --  List of actions to be inserted

      Snn : constant Entity_Id := Make_Temporary (Loc, 'S');
      Pnn : constant Entity_Id := Make_Temporary (Loc, 'P');

   --  Start of processing for Expand_Image_Attribute

   begin
      if Is_Object_Image (Pref) then
         Rewrite_Object_Image (N, Pref, Name_Image, Standard_String);
         return;
      end if;

      --  If Image should be transformed using Put_Image, then do so. See
      --  Exp_Put_Image for details.

      if Exp_Put_Image.Image_Should_Call_Put_Image (N) then
         Rewrite (N, Exp_Put_Image.Build_Image_Call (N));
         Analyze_And_Resolve (N, Standard_String, Suppress => All_Checks);
         return;
      end if;

      Ptyp := Underlying_Type (Entity (Pref));

      --  Ada 2022 allows 'Image on private types, so fetch the underlying
      --  type to obtain the structure of the type. We use the base type,
      --  not the root type for discrete types, to handle properly derived
      --  types, but we use the root type for enumeration types, because the
      --  literal map is attached to the root. Should be inherited ???

      if Is_Real_Type (Ptyp) or else Is_Enumeration_Type (Ptyp) then
         Rtyp := Underlying_Type (Root_Type (Ptyp));
      else
         Rtyp := Underlying_Type (Base_Type (Ptyp));
      end if;

      --  Set Imid (RE_Id of procedure to call), and Tent, target for the
      --  type conversion of the first argument for all possibilities.

      Enum_Case := False;

      if Rtyp = Standard_Boolean then
         --  Use inline expansion if the -gnatd_x switch is not passed to the
         --  compiler. Otherwise expand into a call to the runtime.

         if not Debug_Flag_Underscore_X then
            Expand_Standard_Boolean_Image;
            return;

         else
            Imid := RE_Image_Boolean;
            Tent := Rtyp;
         end if;

      --  For standard character, we have to select the version which handles
      --  soft hyphen correctly, based on the version of Ada in use (this is
      --  ugly, but we have no choice).

      elsif Rtyp = Standard_Character then
         if Ada_Version < Ada_2005 then
            Imid := RE_Image_Character;
         else
            Imid := RE_Image_Character_05;
         end if;

         Tent := Rtyp;

      elsif Rtyp = Standard_Wide_Character then
         Imid := RE_Image_Wide_Character;
         Tent := Rtyp;

      elsif Rtyp = Standard_Wide_Wide_Character then
         Imid := RE_Image_Wide_Wide_Character;
         Tent := Rtyp;

      elsif Is_Signed_Integer_Type (Rtyp) then
         if Esize (Rtyp) <= Standard_Integer_Size then
            Imid := RE_Image_Integer;
            Tent := Standard_Integer;
         elsif Esize (Rtyp) <= Standard_Long_Long_Integer_Size then
            Imid := RE_Image_Long_Long_Integer;
            Tent := Standard_Long_Long_Integer;
         else
            Imid := RE_Image_Long_Long_Long_Integer;
            Tent := Standard_Long_Long_Long_Integer;
         end if;

      elsif Is_Modular_Integer_Type (Rtyp) then
         if Modulus (Rtyp) <= Modulus (RTE (RE_Unsigned)) then
            Imid := RE_Image_Unsigned;
            Tent := RTE (RE_Unsigned);
         elsif Modulus (Rtyp) <= Modulus (RTE (RE_Long_Long_Unsigned)) then
            Imid := RE_Image_Long_Long_Unsigned;
            Tent := RTE (RE_Long_Long_Unsigned);
         else
            Imid := RE_Image_Long_Long_Long_Unsigned;
            Tent := RTE (RE_Long_Long_Long_Unsigned);
         end if;

      elsif Is_Decimal_Fixed_Point_Type (Rtyp) then
         if Esize (Rtyp) <= 32 then
            Imid := RE_Image_Decimal32;
            Tent := RTE (RE_Integer_32);
         elsif Esize (Rtyp) <= 64 then
            Imid := RE_Image_Decimal64;
            Tent := RTE (RE_Integer_64);
         else
            Imid := RE_Image_Decimal128;
            Tent := RTE (RE_Integer_128);
         end if;

      elsif Is_Ordinary_Fixed_Point_Type (Rtyp) then
         declare
            Num : constant Uint := Norm_Num (Small_Value (Rtyp));
            Den : constant Uint := Norm_Den (Small_Value (Rtyp));
            Max : constant Uint := UI_Max (Num, Den);
            Min : constant Uint := UI_Min (Num, Den);
            Siz : constant Uint := Esize (Rtyp);

         begin
            --  Note that we do not use sharp bounds to speed things up

            if Siz <= 32
              and then Max <= Uint_2 ** 31
              and then (Min = Uint_1
                         or else (Num < Den and then Den <= Uint_2 ** 27)
                         or else (Den < Num and then Num <= Uint_2 ** 25))
            then
               Imid := RE_Image_Fixed32;
               Tent := RTE (RE_Integer_32);
            elsif Siz <= 64
              and then Max <= Uint_2 ** 63
              and then (Min = Uint_1
                         or else (Num < Den and then Den <= Uint_2 ** 59)
                         or else (Den < Num and then Num <= Uint_2 ** 53))
            then
               Imid := RE_Image_Fixed64;
               Tent := RTE (RE_Integer_64);
            elsif System_Max_Integer_Size = 128
              and then Max <= Uint_2 ** 127
              and then (Min = Uint_1
                         or else (Num < Den and then Den <= Uint_2 ** 123)
                         or else (Den < Num and then Num <= Uint_2 ** 122))
            then
               Imid := RE_Image_Fixed128;
               Tent := RTE (RE_Integer_128);
            else
               Imid := RE_Image_Fixed;
               Tent := Standard_Long_Float;
            end if;
         end;

      elsif Is_Floating_Point_Type (Rtyp) then
         --  Short_Float and Float are the same type for GNAT

         if Rtyp = Standard_Short_Float or else Rtyp = Standard_Float then
            Imid := RE_Image_Float;
            Tent := Standard_Float;

         elsif Rtyp = Standard_Long_Float then
            Imid := RE_Image_Long_Float;
            Tent := Standard_Long_Float;

         else
            Imid := RE_Image_Long_Long_Float;
            Tent := Standard_Long_Long_Float;
         end if;

      --  Only other possibility is user-defined enumeration type

      else
         pragma Assert (Is_Enumeration_Type (Rtyp));

         if Discard_Names (First_Subtype (Ptyp))
           or else No (Lit_Strings (Rtyp))
         then
            --  When pragma Discard_Names applies to the first subtype, build
            --  (Long_Long_Integer (Pref'Pos (Expr)))'Image. The conversion is
            --  there to avoid applying 'Image directly in Universal_Integer,
            --  which can be a very large type. See also the handling of 'Val.

            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix =>
                  Convert_To (Standard_Long_Long_Integer,
                    Make_Attribute_Reference (Loc,
                    Prefix         => Pref,
                    Attribute_Name => Name_Pos,
                    Expressions    => New_List (Expr))),
                Attribute_Name => Name_Image));
            Analyze_And_Resolve (N, Standard_String);
            return;

         --  Use inline expansion if the -gnatd_x switch is not passed to the
         --  compiler. Otherwise expand into a call to the runtime.

         elsif not Debug_Flag_Underscore_X then
            Expand_User_Defined_Enumeration_Image (Rtyp);
            return;

         else
            Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

            if Ttyp = Standard_Integer_8 then
               Imid := RE_Image_Enumeration_8;

            elsif Ttyp = Standard_Integer_16 then
               Imid := RE_Image_Enumeration_16;

            else
               Imid := RE_Image_Enumeration_32;
            end if;

            --  Apply a validity check, since it is a bit drastic to get a
            --  completely junk image value for an invalid value.

            Insert_Valid_Check (Expr);

            Enum_Case := True;
         end if;
      end if;

      --  Build first argument for call

      if Enum_Case then
         Arg_List := New_List (
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Pos,
             Prefix         => New_Occurrence_Of (Ptyp, Loc),
             Expressions    => New_List (Expr)));

      --  AI12-0020: Ada 2022 allows 'Image for all types, including private
      --  types. If the full type is not a fixed-point type, then it is enough
      --  to set the Conversion_OK flag. However, that would not work for
      --  fixed-point types, because that flag changes the run-time semantics
      --  of fixed-point type conversions; therefore, we must first convert to
      --  Rtyp, and then to Tent.

      else
         declare
            Conv : Node_Id;

         begin
            if Is_Private_Type (Etype (Expr)) then
               if Is_Fixed_Point_Type (Rtyp) then
                  Conv := Convert_To (Tent, OK_Convert_To (Rtyp, Expr));
               else
                  Conv := OK_Convert_To (Tent, Expr);
               end if;
            else
               Conv := Convert_To (Tent, Expr);
            end if;

            Arg_List := New_List (Conv);
         end;
      end if;

      --  Build declarations of Snn and Pnn to be inserted

      Ins_List := New_List (

         --  Snn : String (1 .. typ'Width);

         Make_Object_Declaration (Loc,
            Defining_Identifier => Snn,
            Object_Definition   =>
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Occurrence_Of (Standard_String, Loc),
                Constraint   =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound  => Make_Integer_Literal (Loc, 1),
                        High_Bound =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Rtyp, Loc),
                            Attribute_Name => Name_Width)))))),

         --  Pnn : Natural;

         Make_Object_Declaration (Loc,
           Defining_Identifier => Pnn,
           Object_Definition   => New_Occurrence_Of (Standard_Natural, Loc)));

      --  Append Snn, Pnn arguments

      Append_To (Arg_List, New_Occurrence_Of (Snn, Loc));
      Append_To (Arg_List, New_Occurrence_Of (Pnn, Loc));

      --  Get entity of procedure to call

      Proc_Ent := RTE (Imid);

      --  If the procedure entity is empty, that means we have a case in
      --  no run time mode where the operation is not allowed, and an
      --  appropriate diagnostic has already been issued.

      if No (Proc_Ent) then
         return;
      end if;

      --  Otherwise complete preparation of arguments for run-time call

      --  Add extra arguments for Enumeration case

      if Enum_Case then
         Append_To (Arg_List, New_Occurrence_Of (Lit_Strings (Rtyp), Loc));
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
             Attribute_Name => Name_Address));

      --  For floating-point types, append Digits argument

      elsif Is_Floating_Point_Type (Rtyp) then
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Ptyp, Loc),
             Attribute_Name => Name_Digits));

      --  For decimal, append Scale and also set to do literal conversion

      elsif Is_Decimal_Fixed_Point_Type (Rtyp) then
         Set_Conversion_OK (First (Arg_List));

         Append_To (Arg_List, Make_Integer_Literal (Loc, Scale_Value (Ptyp)));

      --  For ordinary fixed-point types, append Num, Den, Fore, Aft parameters
      --  and also set to do literal conversion.

      elsif Is_Ordinary_Fixed_Point_Type (Rtyp) then
         if Imid /= RE_Image_Fixed then
            Set_Conversion_OK (First (Arg_List));

            Append_To (Arg_List,
              Make_Integer_Literal (Loc, -Norm_Num (Small_Value (Ptyp))));

            Append_To (Arg_List,
              Make_Integer_Literal (Loc, -Norm_Den (Small_Value (Ptyp))));

            --  We want to compute the Fore value for the fixed point type
            --  whose mantissa type is Tent and whose small is typ'Small.

            declare
               T : Ureal := Uint_2 ** (Esize (Tent) - 1) * Small_Value (Ptyp);
               F : Nat   := 2;

            begin
               while T >= Ureal_10 loop
                  F := F + 1;
                  T := T / Ureal_10;
               end loop;

               Append_To (Arg_List,
                  Make_Integer_Literal (Loc, UI_From_Int (F)));
            end;
         end if;

         Append_To (Arg_List, Make_Integer_Literal (Loc, Aft_Value (Ptyp)));

      --  For Wide_Character, append Ada 2005 indication

      elsif Rtyp = Standard_Wide_Character then
         Append_To (Arg_List,
           New_Occurrence_Of
             (Boolean_Literals (Ada_Version >= Ada_2005), Loc));
      end if;

      --  Now append the procedure call to the insert list

      Append_To (Ins_List,
         Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc_Ent, Loc),
          Parameter_Associations => Arg_List));

      --  Insert declarations of Snn, Pnn, and the procedure call. We suppress
      --  checks because we are sure that everything is in range at this stage.

      Insert_Actions (N, Ins_List, Suppress => All_Checks);

      --  Final step is to rewrite the expression as a slice and analyze,
      --  again with no checks, since we are sure that everything is OK.

      Rewrite (N,
        Make_Slice (Loc,
          Prefix         => New_Occurrence_Of (Snn, Loc),
          Discrete_Range =>
            Make_Range (Loc,
              Low_Bound  => Make_Integer_Literal (Loc, 1),
              High_Bound => New_Occurrence_Of (Pnn, Loc))));

      Analyze_And_Resolve (N, Standard_String, Suppress => All_Checks);
   end Expand_Image_Attribute;

   ----------------------------------
   -- Expand_Valid_Value_Attribute --
   ----------------------------------

   procedure Expand_Valid_Value_Attribute (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Args  : constant List_Id    := Expressions (N);
      Btyp  : constant Entity_Id  := Base_Type (Entity (Prefix (N)));
      Rtyp  : constant Entity_Id  := Root_Type (Btyp);
      pragma Assert (Is_Enumeration_Type (Rtyp));

      Func  : RE_Id;
      Ttyp  : Entity_Id;

   begin
      --  Generate:

      --     Valid_Value_Enumeration_NN
      --       (typS, typN'Address, typH'Unrestricted_Access, Num, Is_Wide, X)

      Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

      if Ttyp = Standard_Integer_8 then
         Func := RE_Valid_Value_Enumeration_8;
      elsif Ttyp = Standard_Integer_16 then
         Func := RE_Valid_Value_Enumeration_16;
      else
         Func := RE_Valid_Value_Enumeration_32;
      end if;

      --  The Valid_[Wide_]Wide_Value attribute does not exist

      Prepend_To (Args, New_Occurrence_Of (Standard_False, Loc));

      Prepend_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Rtyp, Loc),
          Attribute_Name => Name_Pos,
          Expressions => New_List (
            Make_Attribute_Reference (Loc,
              Prefix => New_Occurrence_Of (Rtyp, Loc),
              Attribute_Name => Name_Last))));

      if Present (Lit_Hash (Rtyp)) then
         Prepend_To (Args,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Lit_Hash (Rtyp), Loc),
             Attribute_Name => Name_Unrestricted_Access));
      else
         Prepend_To (Args, Make_Null (Loc));
      end if;

      Prepend_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
          Attribute_Name => Name_Address));

      Prepend_To (Args,
        New_Occurrence_Of (Lit_Strings (Rtyp), Loc));

      Rewrite (N,
        Make_Function_Call (Loc,
          Name =>
            New_Occurrence_Of (RTE (Func), Loc),
          Parameter_Associations => Args));

      Analyze_And_Resolve (N, Standard_Boolean);
   end Expand_Valid_Value_Attribute;

   ----------------------------
   -- Expand_Value_Attribute --
   ----------------------------

   --  For scalar types derived from Boolean, Character and integer types
   --  in package Standard, typ'Value (X) expands into:

   --    btyp (Value_xx (X))

   --  where btyp is the base type of the prefix

   --    For types whose root type is Character
   --      xx = Character

   --    For types whose root type is Wide_Character
   --      xx = Wide_Character

   --    For types whose root type is Wide_Wide_Character
   --      xx = Wide_Wide_Character

   --    For types whose root type is Boolean
   --      xx = Boolean

   --    For signed integer types
   --      xx = [Long_Long_[Long_]]Integer

   --    For modular types
   --      xx = [Long_Long_[Long_]]Unsigned

   --    For floating-point types
   --      xx = [Long_[Long_]]Float

   --  For decimal fixed-point types, typ'Value (X) expands into

   --    btyp?(Value_Decimal{32,64,128} (X, typ'Scale));

   --  For the most common ordinary fixed-point types, it expands into

   --    btyp?(Value_Fixed{32,64,128} (X, numerator of S, denominator of S));
   --    where S = typ'Small

   --  For other ordinary fixed-point types, it expands into

   --    btyp (Value_Long_Float (X))

   --  For Wide_[Wide_]Character types, typ'Value (X) expands into

   --    btyp (Value_xx (X, EM))

   --  where btyp is the base type of the prefix, and EM is the encoding method

   --  For enumeration types other than those derived from types Boolean,
   --  Character, Wide_[Wide_]Character in Standard, typ'Value (X) expands to:

   --    Enum'Val
   --      (Value_Enumeration_NN
   --        (typS, typN'Address, typH'Unrestricted_Access, Num, Is_Wide, X))

   --  where typS, typN and typH are the Lit_Strings, Lit_Indexes and Lit_Hash
   --  entities from T's root type entity, and Num is Enum'Pos (Enum'Last).
   --  The Value_Enumeration_NN function will search the tables looking for
   --  X and return the position number in the table if found which is
   --  used to provide the result of 'Value (using Enum'Val). If the
   --  value is not found Constraint_Error is raised. The suffix _NN
   --  depends on the element type of typN.

   procedure Expand_Value_Attribute (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Args  : constant List_Id    := Expressions (N);
      Btyp  : constant Entity_Id  := Etype (N);
      pragma Assert (Is_Base_Type (Btyp));
      pragma Assert (Btyp = Base_Type (Entity (Prefix (N))));
      Rtyp  : constant Entity_Id  := Root_Type (Btyp);

      Is_Wide : Boolean;
      Ttyp    : Entity_Id;
      Vid     : RE_Id;

   begin
      --  Fall through for all cases except user-defined enumeration type
      --  and decimal types, with Vid set to the Id of the entity for the
      --  Value routine and Args set to the list of parameters for the call.

      if Rtyp = Standard_Boolean then
         Vid := RE_Value_Boolean;

      elsif Rtyp = Standard_Character then
         Vid := RE_Value_Character;

      elsif Rtyp = Standard_Wide_Character then
         Vid := RE_Value_Wide_Character;

         Append_To (Args,
           Make_Integer_Literal (Loc,
             Intval => Int (Wide_Character_Encoding_Method)));

      elsif Rtyp = Standard_Wide_Wide_Character then
         Vid := RE_Value_Wide_Wide_Character;

         Append_To (Args,
           Make_Integer_Literal (Loc,
             Intval => Int (Wide_Character_Encoding_Method)));

      elsif Is_Signed_Integer_Type (Rtyp) then
         if Esize (Rtyp) <= Standard_Integer_Size then
            Vid := RE_Value_Integer;
         elsif Esize (Rtyp) <= Standard_Long_Long_Integer_Size then
            Vid := RE_Value_Long_Long_Integer;
         else
            Vid := RE_Value_Long_Long_Long_Integer;
         end if;

      elsif Is_Modular_Integer_Type (Rtyp) then
         if Modulus (Rtyp) <= Modulus (RTE (RE_Unsigned)) then
            Vid := RE_Value_Unsigned;
         elsif Modulus (Rtyp) <= Modulus (RTE (RE_Long_Long_Unsigned)) then
            Vid := RE_Value_Long_Long_Unsigned;
         else
            Vid := RE_Value_Long_Long_Long_Unsigned;
         end if;

      elsif Is_Decimal_Fixed_Point_Type (Rtyp) then
         if Esize (Rtyp) <= 32 and then abs (Scale_Value (Rtyp)) <= 9 then
            Vid := RE_Value_Decimal32;
         elsif Esize (Rtyp) <= 64 and then abs (Scale_Value (Rtyp)) <= 18 then
            Vid := RE_Value_Decimal64;
         else
            Vid := RE_Value_Decimal128;
         end if;

         Append_To (Args, Make_Integer_Literal (Loc, Scale_Value (Rtyp)));

         Rewrite (N,
           OK_Convert_To (Btyp,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (Vid), Loc),
               Parameter_Associations => Args)));

         Set_Etype (N, Btyp);
         Analyze_And_Resolve (N, Btyp);
         return;

      elsif Is_Ordinary_Fixed_Point_Type (Rtyp) then
         declare
            Num : constant Uint := Norm_Num (Small_Value (Rtyp));
            Den : constant Uint := Norm_Den (Small_Value (Rtyp));
            Max : constant Uint := UI_Max (Num, Den);
            Min : constant Uint := UI_Min (Num, Den);
            Siz : constant Uint := Esize (Rtyp);

         begin
            if Siz <= 32
              and then Max <= Uint_2 ** 31
              and then (Min = Uint_1 or else Max <= Uint_2 ** 27)
            then
               Vid := RE_Value_Fixed32;
            elsif Siz <= 64
              and then Max <= Uint_2 ** 63
              and then (Min = Uint_1 or else Max <= Uint_2 ** 59)
            then
               Vid := RE_Value_Fixed64;
            elsif System_Max_Integer_Size = 128
              and then Max <= Uint_2 ** 127
              and then (Min = Uint_1 or else Max <= Uint_2 ** 123)
            then
               Vid := RE_Value_Fixed128;
            else
               Vid := RE_Value_Long_Float;
            end if;

            if Vid /= RE_Value_Long_Float then
               Append_To (Args,
                 Make_Integer_Literal (Loc, -Norm_Num (Small_Value (Rtyp))));

               Append_To (Args,
                 Make_Integer_Literal (Loc, -Norm_Den (Small_Value (Rtyp))));

               Rewrite (N,
                 OK_Convert_To (Btyp,
                   Make_Function_Call (Loc,
                     Name => New_Occurrence_Of (RTE (Vid), Loc),
                     Parameter_Associations => Args)));

               Set_Etype (N, Btyp);
               Analyze_And_Resolve (N, Btyp);
               return;
            end if;
         end;

      elsif Is_Floating_Point_Type (Rtyp) then
         --  Short_Float and Float are the same type for GNAT

         if Rtyp = Standard_Short_Float or else Rtyp = Standard_Float then
            Vid := RE_Value_Float;

         elsif Rtyp = Standard_Long_Float then
            Vid := RE_Value_Long_Float;

         else
            Vid := RE_Value_Long_Long_Float;
         end if;

      --  Only other possibility is user-defined enumeration type

      else
         pragma Assert (Is_Enumeration_Type (Rtyp));

         --  Case of pragma Discard_Names, transform the Value
         --  attribute to Btyp'Val (Long_Long_Integer'Value (Args))

         if Discard_Names (First_Subtype (Btyp))
           or else No (Lit_Strings (Rtyp))
         then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Btyp, Loc),
                Attribute_Name => Name_Val,
                Expressions => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Occurrence_Of (Standard_Long_Long_Integer, Loc),
                    Attribute_Name => Name_Value,
                    Expressions => Args))));

            Analyze_And_Resolve (N, Btyp);

         --  Normal case where we have enumeration tables, build

         --  T'Val
         --   (Value_Enumeration_NN
         --    (typS, typN'Address, typH'Unrestricted_Access, Num, Is_Wide, X))

         else
            Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

            if Ttyp = Standard_Integer_8 then
               Vid := RE_Value_Enumeration_8;
            elsif Ttyp = Standard_Integer_16 then
               Vid := RE_Value_Enumeration_16;
            else
               Vid := RE_Value_Enumeration_32;
            end if;

            if Nkind (First (Args)) = N_Function_Call
              and then Is_Entity_Name (Name (First (Args)))
            then
               declare
                  E : constant Entity_Id := Entity (Name (First (Args)));

               begin
                  Is_Wide := Is_RTE (E, RE_Enum_Wide_String_To_String)
                               or else
                             Is_RTE (E, RE_Enum_Wide_Wide_String_To_String);
               end;

            else
               Is_Wide := False;
            end if;

            Prepend_To (Args,
              New_Occurrence_Of (Boolean_Literals (Is_Wide), Loc));

            Prepend_To (Args,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Rtyp, Loc),
                Attribute_Name => Name_Pos,
                Expressions => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (Rtyp, Loc),
                    Attribute_Name => Name_Last))));

            if Present (Lit_Hash (Rtyp)) then
               Prepend_To (Args,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Lit_Hash (Rtyp), Loc),
                   Attribute_Name => Name_Unrestricted_Access));
            else
               Prepend_To (Args, Make_Null (Loc));
            end if;

            Prepend_To (Args,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
                Attribute_Name => Name_Address));

            Prepend_To (Args,
              New_Occurrence_Of (Lit_Strings (Rtyp), Loc));

            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Btyp, Loc),
                Attribute_Name => Name_Val,
                Expressions => New_List (
                  Make_Function_Call (Loc,
                    Name =>
                      New_Occurrence_Of (RTE (Vid), Loc),
                    Parameter_Associations => Args))));

            Analyze_And_Resolve (N, Btyp);
         end if;

         return;
      end if;

      --  Compiling package Ada.Tags under No_Run_Time_Mode we disable the
      --  expansion of the attribute into the function call statement to avoid
      --  generating spurious errors caused by the use of Integer_Address'Value
      --  in our implementation of Ada.Tags.Internal_Tag.

      if No_Run_Time_Mode
        and then Is_RTE (Rtyp, RE_Integer_Address)
        and then RTU_Loaded (Ada_Tags)
        and then Cunit_Entity (Current_Sem_Unit)
                   = Body_Entity (RTU_Entity (Ada_Tags))
      then
         Rewrite (N,
           Unchecked_Convert_To (Rtyp,
             Make_Integer_Literal (Loc, Uint_0)));

      else
         Rewrite (N,
           Convert_To (Btyp,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (Vid), Loc),
               Parameter_Associations => Args)));
      end if;

      Analyze_And_Resolve (N, Btyp);
   end Expand_Value_Attribute;

   ---------------------------------
   -- Expand_Wide_Image_Attribute --
   ---------------------------------

   --  We expand typ'Wide_Image (X) as follows. First we insert this code:

   --    Rnn : Wide_String (1 .. rt'Wide_Width);
   --    Lnn : Natural;
   --    String_To_Wide_String
   --      (typ'Image (Expr), Rnn, Lnn, Wide_Character_Encoding_Method);

   --  where rt is the root type of the prefix type

   --  Now we replace the Wide_Image reference by

   --    Rnn (1 .. Lnn)

   --  This works in all cases because String_To_Wide_String converts any
   --  wide character escape sequences resulting from the Image call to the
   --  proper Wide_Character equivalent

   --  not quite right for typ = Wide_Character ???

   procedure Expand_Wide_Image_Attribute (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Pref : constant Node_Id    := Prefix (N);
      Rnn  : constant Entity_Id  := Make_Temporary (Loc, 'S');
      Lnn  : constant Entity_Id  := Make_Temporary (Loc, 'P');
      Rtyp : Entity_Id;

   begin
      if Is_Object_Image (Pref) then
         Rewrite_Object_Image (N, Pref, Name_Wide_Image, Standard_Wide_String);
         return;
      end if;

      --  If Image should be transformed using Put_Image, then do so. See
      --  Exp_Put_Image for details.

      if Exp_Put_Image.Image_Should_Call_Put_Image (N) then
         Rewrite (N, Exp_Put_Image.Build_Image_Call (N));
         Analyze_And_Resolve (N, Standard_Wide_String, Suppress => All_Checks);
         return;
      end if;

      Rtyp := Root_Type (Entity (Pref));

      Insert_Actions (N, New_List (

         --  Rnn : Wide_String (1 .. base_typ'Width);

         Make_Object_Declaration (Loc,
            Defining_Identifier => Rnn,
            Object_Definition   =>
              Make_Subtype_Indication (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Standard_Wide_String, Loc),
                Constraint   =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound  => Make_Integer_Literal (Loc, 1),
                        High_Bound =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Rtyp, Loc),
                            Attribute_Name => Name_Wide_Width)))))),

         --  Lnn : Natural;

         Make_Object_Declaration (Loc,
           Defining_Identifier => Lnn,
           Object_Definition   => New_Occurrence_Of (Standard_Natural, Loc)),

         --    String_To_Wide_String
         --      (typ'Image (X), Rnn, Lnn, Wide_Character_Encoding_Method);

         Make_Procedure_Call_Statement (Loc,
           Name =>
             New_Occurrence_Of (RTE (RE_String_To_Wide_String), Loc),

           Parameter_Associations => New_List (
             Make_Attribute_Reference (Loc,
               Prefix         => Prefix (N),
               Attribute_Name => Name_Image,
               Expressions    => Expressions (N)),
             New_Occurrence_Of (Rnn, Loc),
             New_Occurrence_Of (Lnn, Loc),
             Make_Integer_Literal (Loc,
               Intval => Int (Wide_Character_Encoding_Method))))),

         --  Suppress checks because we know everything is properly in range

         Suppress => All_Checks);

      --  Final step is to rewrite the expression as a slice and analyze,
      --  again with no checks, since we are sure that everything is OK.

      Rewrite (N,
        Make_Slice (Loc,
          Prefix         => New_Occurrence_Of (Rnn, Loc),
          Discrete_Range =>
            Make_Range (Loc,
              Low_Bound  => Make_Integer_Literal (Loc, 1),
              High_Bound => New_Occurrence_Of (Lnn, Loc))));

      Analyze_And_Resolve (N, Standard_Wide_String, Suppress => All_Checks);
   end Expand_Wide_Image_Attribute;

   --------------------------------------
   -- Expand_Wide_Wide_Image_Attribute --
   --------------------------------------

   --  We expand typ'Wide_Wide_Image (X) as follows. First we insert this code:

   --    Rnn : Wide_Wide_String (1 .. rt'Wide_Wide_Width);
   --    Lnn : Natural;
   --    String_To_Wide_Wide_String
   --      (typ'Image (Expr), Rnn, Lnn, Wide_Character_Encoding_Method);

   --  where rt is the root type of the prefix type

   --  Now we replace the Wide_Wide_Image reference by

   --    Rnn (1 .. Lnn)

   --  This works in all cases because String_To_Wide_Wide_String converts any
   --  wide character escape sequences resulting from the Image call to the
   --  proper Wide_Wide_Character equivalent

   --  not quite right for typ = Wide_Wide_Character ???

   procedure Expand_Wide_Wide_Image_Attribute (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Pref : constant Node_Id    := Prefix (N);
      Rnn  : constant Entity_Id  := Make_Temporary (Loc, 'S');
      Lnn  : constant Entity_Id  := Make_Temporary (Loc, 'P');
      Rtyp : Entity_Id;

   begin
      if Is_Object_Image (Pref) then
         Rewrite_Object_Image
           (N, Pref, Name_Wide_Wide_Image, Standard_Wide_Wide_String);
         return;
      end if;

      --  If Image should be transformed using Put_Image, then do so. See
      --  Exp_Put_Image for details.

      if Exp_Put_Image.Image_Should_Call_Put_Image (N) then
         Rewrite (N, Exp_Put_Image.Build_Image_Call (N));
         Analyze_And_Resolve
           (N, Standard_Wide_Wide_String, Suppress => All_Checks);
         return;
      end if;

      Rtyp := Root_Type (Entity (Pref));

      Insert_Actions (N, New_List (

         --  Rnn : Wide_Wide_String (1 .. rt'Wide_Wide_Width);

         Make_Object_Declaration (Loc,
            Defining_Identifier => Rnn,
            Object_Definition   =>
              Make_Subtype_Indication (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Standard_Wide_Wide_String, Loc),
                Constraint   =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound  => Make_Integer_Literal (Loc, 1),
                        High_Bound =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Rtyp, Loc),
                            Attribute_Name => Name_Wide_Wide_Width)))))),

         --  Lnn : Natural;

         Make_Object_Declaration (Loc,
           Defining_Identifier => Lnn,
           Object_Definition   => New_Occurrence_Of (Standard_Natural, Loc)),

         --    String_To_Wide_Wide_String
         --      (typ'Image (X), Rnn, Lnn, Wide_Character_Encoding_Method);

         Make_Procedure_Call_Statement (Loc,
           Name =>
             New_Occurrence_Of (RTE (RE_String_To_Wide_Wide_String), Loc),

           Parameter_Associations => New_List (
             Make_Attribute_Reference (Loc,
               Prefix         => Prefix (N),
               Attribute_Name => Name_Image,
               Expressions    => Expressions (N)),
             New_Occurrence_Of (Rnn, Loc),
             New_Occurrence_Of (Lnn, Loc),
             Make_Integer_Literal (Loc,
               Intval => Int (Wide_Character_Encoding_Method))))),

         --  Suppress checks because we know everything is properly in range

         Suppress => All_Checks);

      --  Final step is to rewrite the expression as a slice and analyze,
      --  again with no checks, since we are sure that everything is OK.

      Rewrite (N,
        Make_Slice (Loc,
          Prefix         => New_Occurrence_Of (Rnn, Loc),
          Discrete_Range =>
            Make_Range (Loc,
              Low_Bound  => Make_Integer_Literal (Loc, 1),
              High_Bound => New_Occurrence_Of (Lnn, Loc))));

      Analyze_And_Resolve
        (N, Standard_Wide_Wide_String, Suppress => All_Checks);
   end Expand_Wide_Wide_Image_Attribute;

   ----------------------------
   -- Expand_Width_Attribute --
   ----------------------------

   --  The processing here also handles the case of Wide_[Wide_]Width. With the
   --  exceptions noted, the processing is identical

   --  For scalar types derived from Boolean, character and integer types
   --  in package Standard. Note that the Width attribute is computed at
   --  compile time for all cases except those involving non-static sub-
   --  types. For such subtypes, typ'[Wide_[Wide_]]Width expands into:

   --    Result_Type (xx (yy (Ptyp'First), yy (Ptyp'Last)))

   --  where

   --    For types whose root type is Character
   --      xx = Width_Character
   --      yy = Character

   --    For types whose root type is Wide_Character
   --      xx = Wide_Width_Character
   --      yy = Character

   --    For types whose root type is Wide_Wide_Character
   --      xx = Wide_Wide_Width_Character
   --      yy = Character

   --    For types whose root type is Boolean
   --      xx = Width_Boolean
   --      yy = Boolean

   --    For signed integer types
   --      xx = Width_[Long_Long_[Long_]]Integer
   --      yy = [Long_Long_[Long_]]Integer

   --    For modular integer types
   --      xx = Width_[Long_Long_[Long_]]Unsigned
   --      yy = [Long_Long_[Long_]]Unsigned

   --  For types derived from Wide_Character, typ'Width expands into

   --    Result_Type (Width_Wide_Character (
   --      Wide_Character (typ'First),
   --      Wide_Character (typ'Last),

   --  and typ'Wide_Width expands into:

   --    Result_Type (Wide_Width_Wide_Character (
   --      Wide_Character (typ'First),
   --      Wide_Character (typ'Last));

   --  and typ'Wide_Wide_Width expands into

   --    Result_Type (Wide_Wide_Width_Wide_Character (
   --      Wide_Character (typ'First),
   --      Wide_Character (typ'Last));

   --  For types derived from Wide_Wide_Character, typ'Width expands into

   --    Result_Type (Width_Wide_Wide_Character (
   --      Wide_Wide_Character (typ'First),
   --      Wide_Wide_Character (typ'Last),

   --  and typ'Wide_Width expands into:

   --    Result_Type (Wide_Width_Wide_Wide_Character (
   --      Wide_Wide_Character (typ'First),
   --      Wide_Wide_Character (typ'Last));

   --  and typ'Wide_Wide_Width expands into

   --    Result_Type (Wide_Wide_Width_Wide_Wide_Char (
   --      Wide_Wide_Character (typ'First),
   --      Wide_Wide_Character (typ'Last));

   --  For fixed point types, typ'Width and typ'Wide_[Wide_]Width expand into

   --    if Ptyp'First > Ptyp'Last then 0 else Ptyp'Fore + 1 + Ptyp'Aft end if

   --  and for floating point types, they expand into

   --    if Ptyp'First > Ptyp'Last then 0 else btyp'Width end if

   --  where btyp is the base type. This looks recursive but it isn't
   --  because the base type is always static, and hence the expression
   --  in the else is reduced to an integer literal.

   --  For user-defined enumeration types, typ'Width expands into

   --    Result_Type (Width_Enumeration_NN
   --                  (typS,
   --                   typI'Address,
   --                   typ'Pos (typ'First),
   --                   typ'Pos (Typ'Last)));

   --  and typ'Wide_Width expands into:

   --    Result_Type (Wide_Width_Enumeration_NN
   --                  (typS,
   --                   typI,
   --                   typ'Pos (typ'First),
   --                   typ'Pos (Typ'Last))
   --                   Wide_Character_Encoding_Method);

   --  and typ'Wide_Wide_Width expands into:

   --    Result_Type (Wide_Wide_Width_Enumeration_NN
   --                  (typS,
   --                   typI,
   --                   typ'Pos (typ'First),
   --                   typ'Pos (Typ'Last))
   --                   Wide_Character_Encoding_Method);

   --  where typS and typI are the enumeration image strings and indexes
   --  table, as described in Build_Enumeration_Image_Tables. NN is 8/16/32
   --  for depending on the element type for typI.

   --  Finally if Discard_Names is in effect for an enumeration type, then
   --  a special if expression is built that yields the space needed for the
   --  decimal representation of the largest pos value in the subtype. See
   --  code below for details.

   procedure Expand_Width_Attribute (N : Node_Id; Attr : Atype := Normal) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Pref    : constant Node_Id    := Prefix (N);
      Ptyp    : constant Entity_Id  := Etype (Pref);
      Rtyp    : constant Entity_Id  := Root_Type (Ptyp);
      Arglist : List_Id;
      Ttyp    : Entity_Id;
      XX      : RE_Id;
      YY      : Entity_Id;

   begin
      --  Types derived from Standard.Boolean

      if Rtyp = Standard_Boolean then
         XX := RE_Width_Boolean;
         YY := Rtyp;

      --  Types derived from Standard.Character

      elsif Rtyp = Standard_Character then
         case Attr is
            when Normal    => XX := RE_Width_Character;
            when Wide      => XX := RE_Wide_Width_Character;
            when Wide_Wide => XX := RE_Wide_Wide_Width_Character;
         end case;

         YY := Rtyp;

      --  Types derived from Standard.Wide_Character

      elsif Rtyp = Standard_Wide_Character then
         case Attr is
            when Normal    => XX := RE_Width_Wide_Character;
            when Wide      => XX := RE_Wide_Width_Wide_Character;
            when Wide_Wide => XX := RE_Wide_Wide_Width_Wide_Character;
         end case;

         YY := Rtyp;

      --  Types derived from Standard.Wide_Wide_Character

      elsif Rtyp = Standard_Wide_Wide_Character then
         case Attr is
            when Normal    => XX := RE_Width_Wide_Wide_Character;
            when Wide      => XX := RE_Wide_Width_Wide_Wide_Character;
            when Wide_Wide => XX := RE_Wide_Wide_Width_Wide_Wide_Char;
         end case;

         YY := Rtyp;

      --  Signed integer types

      elsif Is_Signed_Integer_Type (Rtyp) then
         if Esize (Rtyp) <= Standard_Integer_Size then
            XX := RE_Width_Integer;
            YY := Standard_Integer;
         elsif Esize (Rtyp) <= Standard_Long_Long_Integer_Size then
            XX := RE_Width_Long_Long_Integer;
            YY := Standard_Long_Long_Integer;
         else
            XX := RE_Width_Long_Long_Long_Integer;
            YY := Standard_Long_Long_Long_Integer;
         end if;

      --  Modular integer types

      elsif Is_Modular_Integer_Type (Rtyp) then
         if Modulus (Rtyp) <= Modulus (RTE (RE_Unsigned)) then
            XX := RE_Width_Unsigned;
            YY := RTE (RE_Unsigned);
         elsif Modulus (Rtyp) <= Modulus (RTE (RE_Long_Long_Unsigned)) then
            XX := RE_Width_Long_Long_Unsigned;
            YY := RTE (RE_Long_Long_Unsigned);
         else
            XX := RE_Width_Long_Long_Long_Unsigned;
            YY := RTE (RE_Long_Long_Long_Unsigned);
         end if;

      --  Fixed point types

      elsif Is_Fixed_Point_Type (Rtyp) then
         Rewrite (N,
           Make_If_Expression (Loc,
             Expressions => New_List (

               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ptyp, Loc),
                     Attribute_Name => Name_First),

                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ptyp, Loc),
                     Attribute_Name => Name_Last)),

               Make_Integer_Literal (Loc, 0),

               Make_Op_Add (Loc,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Ptyp, Loc),
                   Attribute_Name => Name_Fore),

                 Make_Op_Add (Loc,
                   Make_Integer_Literal (Loc, 1),
                   Make_Integer_Literal (Loc, Aft_Value (Ptyp)))))));

         Analyze_And_Resolve (N, Typ);
         return;

      --  Floating point types

      elsif Is_Floating_Point_Type (Rtyp) then
         Rewrite (N,
           Make_If_Expression (Loc,
             Expressions => New_List (

               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ptyp, Loc),
                     Attribute_Name => Name_First),

                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ptyp, Loc),
                     Attribute_Name => Name_Last)),

               Make_Integer_Literal (Loc, 0),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Base_Type (Ptyp), Loc),
                 Attribute_Name => Name_Width))));

         Analyze_And_Resolve (N, Typ);
         return;

      --  User-defined enumeration types

      else
         pragma Assert (Is_Enumeration_Type (Rtyp));

         --  Whenever pragma Discard_Names is in effect, the value we need
         --  is the value needed to accommodate the largest integer pos value
         --  in the range of the subtype + 1 for the space at the start. We
         --  build:

         --     Tnn : constant Integer := Rtyp'Pos (Ptyp'Last);

         --  and replace the expression by

         --     (if Ptyp'Range_Length = 0 then 0
         --      else (if Tnn < 10 then 2
         --            else (if Tnn < 100 then 3
         --                  ...
         --                      else n)))...

         --  where n is equal to Rtyp'Pos (Ptyp'Last) + 1

         --  Note: The above processing is in accordance with the intent of
         --  the RM, which is that Width should be related to the impl-defined
         --  behavior of Image. It is not clear what this means if Image is
         --  not defined (as in the configurable run-time case for GNAT) and
         --  gives an error at compile time.

         --  We choose in this case to just go ahead and implement Width the
         --  same way, returning what Image would have returned if it has been
         --  available in the configurable run-time library.

         if Discard_Names (Rtyp) then
            declare
               Tnn   : constant Entity_Id := Make_Temporary (Loc, 'T');
               Cexpr : Node_Id;

               P : constant Nat :=
                 UI_To_Int (Enumeration_Pos (Entity (Type_High_Bound (Rtyp))));
               --  The largest value that might need to be represented

               K : Pos;
               M : Pos;
               --  K is the number of chars that will fit the image of 0..M-1;
               --  M is the smallest number that won't fit in K chars.

            begin
               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Tnn,
                   Constant_Present    => True,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Integer, Loc),
                   Expression =>
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Rtyp, Loc),
                       Attribute_Name => Name_Pos,
                       Expressions    => New_List (
                         Convert_To (Rtyp,
                           Make_Attribute_Reference (Loc,
                             Prefix         => New_Occurrence_Of (Ptyp, Loc),
                             Attribute_Name => Name_Last))))));

               --  OK, now we need to build the if expression. First get the
               --  values of K and M for the largest possible value P.

               K := 2;
               M := 10;
               --  With 2 characters we can represent values in 0..9

               while P >= M loop
                  M := M * 10;
                  K := K + 1;
               end loop;

               --  Build inner else

               Cexpr := Make_Integer_Literal (Loc, K);

               --  Wrap in inner if's until counted down to 2

               while K > 2 loop
                  M := M / 10;
                  K := K - 1;

                  Cexpr :=
                    Make_If_Expression (Loc,
                      Expressions => New_List (
                        Make_Op_Lt (Loc,
                          Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                          Right_Opnd => Make_Integer_Literal (Loc, M)),
                        Make_Integer_Literal (Loc, K),
                        Cexpr));
               end loop;

               --  Add initial comparison for null range and we are done, so
               --  rewrite the attribute occurrence with this expression.

               Rewrite (N,
                 Convert_To (Typ,
                   Make_If_Expression (Loc,
                     Expressions => New_List (
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Make_Attribute_Reference (Loc,
                             Prefix         => New_Occurrence_Of (Ptyp, Loc),
                             Attribute_Name => Name_Range_Length),
                         Right_Opnd => Make_Integer_Literal (Loc, 0)),
                       Make_Integer_Literal (Loc, 0),
                       Cexpr))));

               Analyze_And_Resolve (N, Typ);
               return;
            end;
         end if;

         --  Normal case, not Discard_Names

         Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

         case Attr is
            when Normal =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16 then
                  XX := RE_Width_Enumeration_16;
               else
                  XX := RE_Width_Enumeration_32;
               end if;

            when Wide =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Wide_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16 then
                  XX := RE_Wide_Width_Enumeration_16;
               else
                  XX := RE_Wide_Width_Enumeration_32;
               end if;

            when Wide_Wide =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Wide_Wide_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16 then
                  XX := RE_Wide_Wide_Width_Enumeration_16;
               else
                  XX := RE_Wide_Wide_Width_Enumeration_32;
               end if;
         end case;

         Arglist :=
           New_List (
             New_Occurrence_Of (Lit_Strings (Rtyp), Loc),

             Make_Attribute_Reference (Loc,
               Prefix => New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
               Attribute_Name => Name_Address),

             Make_Attribute_Reference (Loc,
               Prefix => New_Occurrence_Of (Ptyp, Loc),
               Attribute_Name => Name_Pos,

               Expressions => New_List (
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Ptyp, Loc),
                   Attribute_Name => Name_First))),

             Make_Attribute_Reference (Loc,
               Prefix => New_Occurrence_Of (Ptyp, Loc),
               Attribute_Name => Name_Pos,

               Expressions => New_List (
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Ptyp, Loc),
                   Attribute_Name => Name_Last))));

         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (XX), Loc),
               Parameter_Associations => Arglist)));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  If we fall through XX and YY are set

      Arglist := New_List (
        Convert_To (YY,
          Make_Attribute_Reference (Loc,
            Prefix => New_Occurrence_Of (Ptyp, Loc),
            Attribute_Name => Name_First)),

        Convert_To (YY,
          Make_Attribute_Reference (Loc,
            Prefix => New_Occurrence_Of (Ptyp, Loc),
            Attribute_Name => Name_Last)));

      Rewrite (N,
        Convert_To (Typ,
          Make_Function_Call (Loc,
            Name => New_Occurrence_Of (RTE (XX), Loc),
            Parameter_Associations => Arglist)));

      Analyze_And_Resolve (N, Typ);
   end Expand_Width_Attribute;

   --------------------------
   -- Rewrite_Object_Image --
   --------------------------

   procedure Rewrite_Object_Image
     (N         : Node_Id;
      Pref      : Node_Id;
      Attr_Name : Name_Id;
      Str_Typ   : Entity_Id)
   is
      P    : Node_Id;
      Ptyp : Entity_Id;

   begin
      P    := Pref;
      Ptyp := Etype (P);

      --  If the type of the prefix is universal integer, which is a very large
      --  type, try to compute a narrower type. This may happen when the prefix
      --  itself is an attribute returning universal integer or a named number.

      if Ptyp = Universal_Integer then
         if Nkind (P) in N_Type_Conversion | N_Unchecked_Type_Conversion then
            P    := Expression (P);
            Ptyp := Etype (P);

         elsif Nkind (P) = N_Integer_Literal then
            declare
               Val  : constant Uint := Intval (P);
               Siz  : constant Nat  := Type_Size_For (Val);

            begin
               if Siz <= System_Max_Integer_Size then
                  Ptyp := Integer_Type_For (UI_From_Int (Siz), Val >= Uint_0);
               end if;
            end;
         end if;
      end if;

      --  If the prefix is a component that depends on a discriminant, then
      --  create an actual subtype for it.

      if Nkind (P) = N_Selected_Component then
         declare
            Decl : constant Node_Id :=
                     Build_Actual_Subtype_Of_Component (Ptyp, P);
         begin
            if Present (Decl) then
               Insert_Action (N, Decl);
               Ptyp := Defining_Identifier (Decl);
            end if;
         end;
      end if;

      Rewrite (N,
        Make_Attribute_Reference (Sloc (N),
          Prefix         => New_Occurrence_Of (Ptyp, Sloc (N)),
          Attribute_Name => Attr_Name,
          Expressions    => New_List (Unchecked_Convert_To (Ptyp, P))));

      Analyze_And_Resolve (N, Str_Typ);
   end Rewrite_Object_Image;
end Exp_Imgv;
