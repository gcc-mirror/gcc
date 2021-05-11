------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           E I N F O . U T I L S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2020-2021, Free Software Foundation, Inc.        --
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
with Elists;         use Elists;
with Nlists;         use Nlists;
with Output;         use Output;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;

package body Einfo.Utils is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Has_Option
     (State_Id   : Entity_Id;
      Option_Nam : Name_Id) return Boolean;
   --  Determine whether abstract state State_Id has particular option denoted
   --  by the name Option_Nam.

   ----------------
   -- Has_Option --
   ----------------

   function Has_Option
     (State_Id   : Entity_Id;
      Option_Nam : Name_Id) return Boolean
   is
      Decl    : constant Node_Id := Parent (State_Id);
      Opt     : Node_Id;
      Opt_Nam : Node_Id;

   begin
      pragma Assert (Ekind (State_Id) = E_Abstract_State);

      --  The declaration of abstract states with options appear as an
      --  extension aggregate. If this is not the case, the option is not
      --  available.

      if Nkind (Decl) /= N_Extension_Aggregate then
         return False;
      end if;

      --  Simple options

      Opt := First (Expressions (Decl));
      while Present (Opt) loop
         if Nkind (Opt) = N_Identifier and then Chars (Opt) = Option_Nam then
            return True;
         end if;

         Next (Opt);
      end loop;

      --  Complex options with various specifiers

      Opt := First (Component_Associations (Decl));
      while Present (Opt) loop
         Opt_Nam := First (Choices (Opt));

         if Nkind (Opt_Nam) = N_Identifier
           and then Chars (Opt_Nam) = Option_Nam
         then
            return True;
         end if;

         Next (Opt);
      end loop;

      return False;
   end Has_Option;

   ------------------------------
   -- Classification Functions --
   ------------------------------

   function Is_Access_Object_Type               (Id : E) return B is
   begin
      return Is_Access_Type (Id)
        and then Ekind (Directly_Designated_Type (Id)) /= E_Subprogram_Type;
   end Is_Access_Object_Type;

   function Is_Access_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Access_Kind;
   end Is_Access_Type;

   function Is_Access_Protected_Subprogram_Type (Id : E) return B is
   begin
      return Ekind (Id) in Access_Protected_Kind;
   end Is_Access_Protected_Subprogram_Type;

   function Is_Access_Subprogram_Type           (Id : E) return B is
   begin
      return Is_Access_Type (Id)
        and then Ekind (Directly_Designated_Type (Id)) = E_Subprogram_Type;
   end Is_Access_Subprogram_Type;

   function Is_Aggregate_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Aggregate_Kind;
   end Is_Aggregate_Type;

   function Is_Anonymous_Access_Type            (Id : E) return B is
   begin
      return Ekind (Id) in Anonymous_Access_Kind;
   end Is_Anonymous_Access_Type;

   function Is_Array_Type                       (Id : E) return B is
   begin
      return Ekind (Id) in Array_Kind;
   end Is_Array_Type;

   function Is_Assignable                       (Id : E) return B is
   begin
      return Ekind (Id) in Assignable_Kind;
   end Is_Assignable;

   function Is_Class_Wide_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Class_Wide_Kind;
   end Is_Class_Wide_Type;

   function Is_Composite_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Composite_Kind;
   end Is_Composite_Type;

   function Is_Concurrent_Body                  (Id : E) return B is
   begin
      return Ekind (Id) in Concurrent_Body_Kind;
   end Is_Concurrent_Body;

   function Is_Concurrent_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Concurrent_Kind;
   end Is_Concurrent_Type;

   function Is_Decimal_Fixed_Point_Type         (Id : E) return B is
   begin
      return Ekind (Id) in Decimal_Fixed_Point_Kind;
   end Is_Decimal_Fixed_Point_Type;

   function Is_Digits_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Digits_Kind;
   end Is_Digits_Type;

   function Is_Discrete_Or_Fixed_Point_Type     (Id : E) return B is
   begin
      return Ekind (Id) in Discrete_Or_Fixed_Point_Kind;
   end Is_Discrete_Or_Fixed_Point_Type;

   function Is_Discrete_Type                    (Id : E) return B is
   begin
      return Ekind (Id) in Discrete_Kind;
   end Is_Discrete_Type;

   function Is_Elementary_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Elementary_Kind;
   end Is_Elementary_Type;

   function Is_Entry                            (Id : E) return B is
   begin
      return Ekind (Id) in Entry_Kind;
   end Is_Entry;

   function Is_Enumeration_Type                 (Id : E) return B is
   begin
      return Ekind (Id) in Enumeration_Kind;
   end Is_Enumeration_Type;

   function Is_Fixed_Point_Type                 (Id : E) return B is
   begin
      return Ekind (Id) in Fixed_Point_Kind;
   end Is_Fixed_Point_Type;

   function Is_Floating_Point_Type              (Id : E) return B is
   begin
      return Ekind (Id) in Float_Kind;
   end Is_Floating_Point_Type;

   function Is_Formal                           (Id : E) return B is
   begin
      return Ekind (Id) in Formal_Kind;
   end Is_Formal;

   function Is_Formal_Object                    (Id : E) return B is
   begin
      return Ekind (Id) in Formal_Object_Kind;
   end Is_Formal_Object;

   function Is_Generic_Subprogram               (Id : E) return B is
   begin
      return Ekind (Id) in Generic_Subprogram_Kind;
   end Is_Generic_Subprogram;

   function Is_Generic_Unit                     (Id : E) return B is
   begin
      return Ekind (Id) in Generic_Unit_Kind;
   end Is_Generic_Unit;

   function Is_Ghost_Entity (Id : Entity_Id) return Boolean is
   begin
      return Is_Checked_Ghost_Entity (Id) or else Is_Ignored_Ghost_Entity (Id);
   end Is_Ghost_Entity;

   function Is_Incomplete_Or_Private_Type       (Id : E) return B is
   begin
      return Ekind (Id) in Incomplete_Or_Private_Kind;
   end Is_Incomplete_Or_Private_Type;

   function Is_Incomplete_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Incomplete_Kind;
   end Is_Incomplete_Type;

   function Is_Integer_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Integer_Kind;
   end Is_Integer_Type;

   function Is_Modular_Integer_Type             (Id : E) return B is
   begin
      return Ekind (Id) in Modular_Integer_Kind;
   end Is_Modular_Integer_Type;

   function Is_Named_Access_Type                (Id : E) return B is
   begin
      return Ekind (Id) in E_Access_Type .. -- ????
                             E_Access_Protected_Subprogram_Type;
   end Is_Named_Access_Type;

   function Is_Named_Number                     (Id : E) return B is
   begin
      return Ekind (Id) in Named_Kind;
   end Is_Named_Number;

   function Is_Numeric_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Numeric_Kind;
   end Is_Numeric_Type;

   function Is_Object                           (Id : E) return B is
   begin
      return Ekind (Id) in Object_Kind;
   end Is_Object;

   function Is_Ordinary_Fixed_Point_Type        (Id : E) return B is
   begin
      return Ekind (Id) in Ordinary_Fixed_Point_Kind;
   end Is_Ordinary_Fixed_Point_Type;

   function Is_Overloadable                     (Id : E) return B is
   begin
      return Ekind (Id) in Overloadable_Kind;
   end Is_Overloadable;

   function Is_Private_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Private_Kind;
   end Is_Private_Type;

   function Is_Protected_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Protected_Kind;
   end Is_Protected_Type;

   function Is_Real_Type                        (Id : E) return B is
   begin
      return Ekind (Id) in Real_Kind;
   end Is_Real_Type;

   function Is_Record_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Record_Kind;
   end Is_Record_Type;

   function Is_Scalar_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Scalar_Kind;
   end Is_Scalar_Type;

   function Is_Signed_Integer_Type              (Id : E) return B is
   begin
      return Ekind (Id) in Signed_Integer_Kind;
   end Is_Signed_Integer_Type;

   function Is_Subprogram                       (Id : E) return B is
   begin
      return Ekind (Id) in Subprogram_Kind;
   end Is_Subprogram;

   function Is_Subprogram_Or_Entry              (Id : E) return B is
   begin
      return Ekind (Id) in Subprogram_Kind
               or else
             Ekind (Id) in Entry_Kind;
   end Is_Subprogram_Or_Entry;

   function Is_Subprogram_Or_Generic_Subprogram (Id : E) return B is
   begin
      return Ekind (Id) in Subprogram_Kind
               or else
             Ekind (Id) in Generic_Subprogram_Kind;
   end Is_Subprogram_Or_Generic_Subprogram;

   function Is_Task_Type                        (Id : E) return B is
   begin
      return Ekind (Id) in Task_Kind;
   end Is_Task_Type;

   function Is_Type                             (Id : E) return B is
   begin
      return Ekind (Id) in Type_Kind;
   end Is_Type;

   -----------------------------------
   -- Field Initialization Routines --
   -----------------------------------

   procedure Init_Alignment (Id : E) is
   begin
      Set_Alignment (Id, Uint_0);
   end Init_Alignment;

   procedure Init_Alignment (Id : E; V : Int) is
   begin
      Set_Alignment (Id, UI_From_Int (V));
   end Init_Alignment;

   procedure Init_Component_Bit_Offset (Id : E) is
   begin
      Set_Component_Bit_Offset (Id, No_Uint);
   end Init_Component_Bit_Offset;

   procedure Init_Component_Bit_Offset (Id : E; V : Int) is
   begin
      Set_Component_Bit_Offset (Id, UI_From_Int (V));
   end Init_Component_Bit_Offset;

   procedure Init_Component_Size (Id : E) is
   begin
      Set_Component_Size (Id, Uint_0);
   end Init_Component_Size;

   procedure Init_Component_Size (Id : E; V : Int) is
   begin
      Set_Component_Size (Id, UI_From_Int (V));
   end Init_Component_Size;

   procedure Init_Digits_Value (Id : E) is
   begin
      Set_Digits_Value (Id, Uint_0);
   end Init_Digits_Value;

   procedure Init_Digits_Value (Id : E; V : Int) is
   begin
      Set_Digits_Value (Id, UI_From_Int (V));
   end Init_Digits_Value;

   procedure Init_Esize (Id : E) is
   begin
      Set_Esize (Id, Uint_0);
   end Init_Esize;

   procedure Init_Esize (Id : E; V : Int) is
   begin
      Set_Esize (Id, UI_From_Int (V));
   end Init_Esize;

   procedure Init_Normalized_First_Bit (Id : E) is
   begin
      Set_Normalized_First_Bit (Id, No_Uint);
   end Init_Normalized_First_Bit;

   procedure Init_Normalized_First_Bit (Id : E; V : Int) is
   begin
      Set_Normalized_First_Bit (Id, UI_From_Int (V));
   end Init_Normalized_First_Bit;

   procedure Init_Normalized_Position (Id : E) is
   begin
      Set_Normalized_Position (Id, No_Uint);
   end Init_Normalized_Position;

   procedure Init_Normalized_Position (Id : E; V : Int) is
   begin
      Set_Normalized_Position (Id, UI_From_Int (V));
   end Init_Normalized_Position;

   procedure Init_Normalized_Position_Max (Id : E) is
   begin
      Set_Normalized_Position_Max (Id, No_Uint);
   end Init_Normalized_Position_Max;

   procedure Init_Normalized_Position_Max (Id : E; V : Int) is
   begin
      Set_Normalized_Position_Max (Id, UI_From_Int (V));
   end Init_Normalized_Position_Max;

   procedure Init_RM_Size (Id : E) is
   begin
      Set_RM_Size (Id, Uint_0);
   end Init_RM_Size;

   procedure Init_RM_Size (Id : E; V : Int) is
   begin
      Set_RM_Size (Id, UI_From_Int (V));
   end Init_RM_Size;

   -----------------------------
   -- Init_Component_Location --
   -----------------------------

   procedure Init_Component_Location (Id : E) is
   begin
      Set_Normalized_First_Bit  (Id, No_Uint);
      Set_Normalized_Position_Max (Id, No_Uint);
      Set_Component_Bit_Offset (Id, No_Uint);
      Set_Esize (Id, Uint_0);
      Set_Normalized_Position (Id, No_Uint);
   end Init_Component_Location;

   ----------------------------
   -- Init_Object_Size_Align --
   ----------------------------

   procedure Init_Object_Size_Align (Id : E) is
   begin
      Set_Esize (Id, Uint_0);
      Set_Alignment (Id, Uint_0);
   end Init_Object_Size_Align;

   ---------------
   -- Init_Size --
   ---------------

   procedure Init_Size (Id : E; V : Int) is
   begin
      pragma Assert (not Is_Object (Id));
      Set_Esize (Id, UI_From_Int (V));
      Set_RM_Size (Id, UI_From_Int (V));
   end Init_Size;

   ---------------------
   -- Init_Size_Align --
   ---------------------

   procedure Init_Size_Align (Id : E) is
   begin
      pragma Assert (not Is_Object (Id));
      Set_Esize (Id, Uint_0);
      Set_RM_Size (Id, Uint_0);
      Set_Alignment (Id, Uint_0);
   end Init_Size_Align;

   ----------------------------------------------
   -- Type Representation Attribute Predicates --
   ----------------------------------------------

   function Known_Alignment                       (E : Entity_Id) return B is
   begin
      return Alignment (E) /= Uint_0
        and then Alignment (E) /= No_Uint;
   end Known_Alignment;

   function Known_Component_Bit_Offset            (E : Entity_Id) return B is
   begin
      return Component_Bit_Offset (E) /= No_Uint;
   end Known_Component_Bit_Offset;

   function Known_Component_Size                  (E : Entity_Id) return B is
   begin
      return Component_Size (Base_Type (E)) /= Uint_0
        and then Component_Size (Base_Type (E)) /= No_Uint;
   end Known_Component_Size;

   function Known_Esize                           (E : Entity_Id) return B is
   begin
      return Esize (E) /= Uint_0
        and then Esize (E) /= No_Uint;
   end Known_Esize;

   function Known_Normalized_First_Bit            (E : Entity_Id) return B is
   begin
      return Normalized_First_Bit (E) /= No_Uint;
   end Known_Normalized_First_Bit;

   function Known_Normalized_Position             (E : Entity_Id) return B is
   begin
      return Normalized_Position (E) /= No_Uint;
   end Known_Normalized_Position;

   function Known_Normalized_Position_Max         (E : Entity_Id) return B is
   begin
      return Normalized_Position_Max (E) /= No_Uint;
   end Known_Normalized_Position_Max;

   function Known_RM_Size                         (E : Entity_Id) return B is
   begin
      return RM_Size (E) /= No_Uint
        and then (RM_Size (E) /= Uint_0
                    or else Is_Discrete_Type (E)
                    or else Is_Fixed_Point_Type (E));
   end Known_RM_Size;

   function Known_Static_Component_Bit_Offset     (E : Entity_Id) return B is
   begin
      return Component_Bit_Offset (E) /= No_Uint
        and then Component_Bit_Offset (E) >= Uint_0;
   end Known_Static_Component_Bit_Offset;

   function Known_Static_Component_Size           (E : Entity_Id) return B is
   begin
      return Component_Size (Base_Type (E)) > Uint_0;
   end Known_Static_Component_Size;

   function Known_Static_Esize                    (E : Entity_Id) return B is
   begin
      return Esize (E) > Uint_0
        and then not Is_Generic_Type (E);
   end Known_Static_Esize;

   function Known_Static_Normalized_First_Bit     (E : Entity_Id) return B is
   begin
      return Normalized_First_Bit (E) /= No_Uint
        and then Normalized_First_Bit (E) >= Uint_0;
   end Known_Static_Normalized_First_Bit;

   function Known_Static_Normalized_Position      (E : Entity_Id) return B is
   begin
      return Normalized_Position (E) /= No_Uint
        and then Normalized_Position (E) >= Uint_0;
   end Known_Static_Normalized_Position;

   function Known_Static_Normalized_Position_Max  (E : Entity_Id) return B is
   begin
      return Normalized_Position_Max (E) /= No_Uint
        and then Normalized_Position_Max (E) >= Uint_0;
   end Known_Static_Normalized_Position_Max;

   function Known_Static_RM_Size                  (E : Entity_Id) return B is
   begin
      return (RM_Size (E) > Uint_0
                or else Is_Discrete_Type (E)
                or else Is_Fixed_Point_Type (E))
        and then not Is_Generic_Type (E);
   end Known_Static_RM_Size;

   function Unknown_Alignment                     (E : Entity_Id) return B is
   begin
      return Alignment (E) = Uint_0
        or else Alignment (E) = No_Uint;
   end Unknown_Alignment;

   function Unknown_Component_Bit_Offset          (E : Entity_Id) return B is
   begin
      return Component_Bit_Offset (E) = No_Uint;
   end Unknown_Component_Bit_Offset;

   function Unknown_Component_Size                (E : Entity_Id) return B is
   begin
      return Component_Size (Base_Type (E)) = Uint_0
               or else
             Component_Size (Base_Type (E)) = No_Uint;
   end Unknown_Component_Size;

   function Unknown_Esize                         (E : Entity_Id) return B is
   begin
      return Esize (E) = No_Uint
               or else
             Esize (E) = Uint_0;
   end Unknown_Esize;

   function Unknown_Normalized_First_Bit          (E : Entity_Id) return B is
   begin
      return Normalized_First_Bit (E) = No_Uint;
   end Unknown_Normalized_First_Bit;

   function Unknown_Normalized_Position           (E : Entity_Id) return B is
   begin
      return Normalized_Position (E) = No_Uint;
   end Unknown_Normalized_Position;

   function Unknown_Normalized_Position_Max       (E : Entity_Id) return B is
   begin
      return Normalized_Position_Max (E) = No_Uint;
   end Unknown_Normalized_Position_Max;

   function Unknown_RM_Size                       (E : Entity_Id) return B is
   begin
      return (RM_Size (E) = Uint_0
                and then not Is_Discrete_Type (E)
                and then not Is_Fixed_Point_Type (E))
        or else RM_Size (E) = No_Uint;
   end Unknown_RM_Size;

   --------------------
   -- Address_Clause --
   --------------------

   function Address_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Address);
   end Address_Clause;

   ---------------
   -- Aft_Value --
   ---------------

   function Aft_Value (Id : E) return U is
      Result    : Nat := 1;
      Delta_Val : Ureal := Delta_Value (Id);
   begin
      while Delta_Val < Ureal_Tenth loop
         Delta_Val := Delta_Val * Ureal_10;
         Result := Result + 1;
      end loop;

      return UI_From_Int (Result);
   end Aft_Value;

   ----------------------
   -- Alignment_Clause --
   ----------------------

   function Alignment_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Alignment);
   end Alignment_Clause;

   -------------------
   -- Append_Entity --
   -------------------

   procedure Append_Entity (Id : Entity_Id; Scop : Entity_Id) is
      Last : constant Entity_Id := Last_Entity (Scop);

   begin
      Set_Scope (Id, Scop);
      Set_Prev_Entity (Id, Empty);  --  Empty <-- Id

      --  The entity chain is empty

      if No (Last) then
         Set_First_Entity (Scop, Id);

      --  Otherwise the entity chain has at least one element

      else
         Link_Entities (Last, Id);  --  Last <-- Id, Last --> Id
      end if;

      --  NOTE: The setting of the Next_Entity attribute of Id must happen
      --  here as opposed to at the beginning of the routine because doing
      --  so causes the binder to hang. It is not clear why ???

      Set_Next_Entity (Id, Empty);  --  Id --> Empty

      Set_Last_Entity (Scop, Id);
   end Append_Entity;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : E) return E is
   begin
      if Is_Base_Type (Id) then
         return Id;
      else
         pragma Assert (Is_Type (Id));
         return Etype (Id);
      end if;
   end Base_Type;

   ----------------------
   -- Declaration_Node --
   ----------------------

   function Declaration_Node (Id : E) return N is
      P : Node_Id;

   begin
      if Ekind (Id) = E_Incomplete_Type
        and then Present (Full_View (Id))
      then
         P := Parent (Full_View (Id));
      else
         P := Parent (Id);
      end if;

      loop
         if Nkind (P) in N_Selected_Component | N_Expanded_Name
           or else (Nkind (P) = N_Defining_Program_Unit_Name
                     and then Is_Child_Unit (Id))
         then
            P := Parent (P);
         else
            return P;
         end if;
      end loop;
   end Declaration_Node;

   ---------------------
   -- Designated_Type --
   ---------------------

   function Designated_Type (Id : E) return E is
      Desig_Type : Entity_Id;

   begin
      Desig_Type := Directly_Designated_Type (Id);

      if No (Desig_Type) then
         pragma Assert (Error_Posted (Id));
         return Any_Type;
      end if;

      if Is_Incomplete_Type (Desig_Type)
        and then Present (Full_View (Desig_Type))
      then
         return Full_View (Desig_Type);
      end if;

      if Is_Class_Wide_Type (Desig_Type)
        and then Is_Incomplete_Type (Etype (Desig_Type))
        and then Present (Full_View (Etype (Desig_Type)))
        and then Present (Class_Wide_Type (Full_View (Etype (Desig_Type))))
      then
         return Class_Wide_Type (Full_View (Etype (Desig_Type)));
      end if;

      return Desig_Type;
   end Designated_Type;

   ----------------------
   -- Entry_Index_Type --
   ----------------------

   function Entry_Index_Type (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Entry_Family);
      return Etype (Discrete_Subtype_Definition (Parent (Id)));
   end Entry_Index_Type;

   ---------------------
   -- First_Component --
   ---------------------

   function First_Component (Id : E) return E is
      Comp_Id : Entity_Id;

   begin
      pragma Assert
        (Is_Concurrent_Type (Id)
          or else Is_Incomplete_Or_Private_Type (Id)
          or else Is_Record_Type (Id));

      Comp_Id := First_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) = E_Component;
         Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end First_Component;

   -------------------------------------
   -- First_Component_Or_Discriminant --
   -------------------------------------

   function First_Component_Or_Discriminant (Id : E) return E is
      Comp_Id : Entity_Id;

   begin
      pragma Assert
        (Is_Concurrent_Type (Id)
          or else Is_Incomplete_Or_Private_Type (Id)
          or else Is_Record_Type (Id)
          or else Has_Discriminants (Id));

      Comp_Id := First_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) in E_Component | E_Discriminant;
         Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end First_Component_Or_Discriminant;

   ------------------
   -- First_Formal --
   ------------------

   function First_Formal (Id : E) return E is
      Formal : Entity_Id;

   begin
      pragma Assert
        (Is_Generic_Subprogram (Id)
           or else Is_Overloadable (Id)
           or else Ekind (Id) in E_Entry_Family
                               | E_Subprogram_Body
                               | E_Subprogram_Type);

      if Ekind (Id) = E_Enumeration_Literal then
         return Empty;

      else
         Formal := First_Entity (Id);

         --  Deal with the common, non-generic case first

         if No (Formal) or else Is_Formal (Formal) then
            return Formal;
         end if;

         --  The first/next entity chain of a generic subprogram contains all
         --  generic formal parameters, followed by the formal parameters.

         if Is_Generic_Subprogram (Id) then
            while Present (Formal) and then not Is_Formal (Formal) loop
               Next_Entity (Formal);
            end loop;
            return Formal;
         else
            return Empty;
         end if;
      end if;
   end First_Formal;

   ------------------------------
   -- First_Formal_With_Extras --
   ------------------------------

   function First_Formal_With_Extras (Id : E) return E is
      Formal : Entity_Id;

   begin
      pragma Assert
        (Is_Generic_Subprogram (Id)
           or else Is_Overloadable (Id)
           or else Ekind (Id) in E_Entry_Family
                               | E_Subprogram_Body
                               | E_Subprogram_Type);

      if Ekind (Id) = E_Enumeration_Literal then
         return Empty;

      else
         Formal := First_Entity (Id);

         --  The first/next entity chain of a generic subprogram contains all
         --  generic formal parameters, followed by the formal parameters. Go
         --  directly to the parameters by skipping the formal part.

         if Is_Generic_Subprogram (Id) then
            while Present (Formal) and then not Is_Formal (Formal) loop
               Next_Entity (Formal);
            end loop;
         end if;

         if Present (Formal) and then Is_Formal (Formal) then
            return Formal;
         else
            return Extra_Formals (Id);  -- Empty if no extra formals
         end if;
      end if;
   end First_Formal_With_Extras;

   -------------------------------------
   -- Get_Attribute_Definition_Clause --
   -------------------------------------

   function Get_Attribute_Definition_Clause
     (E  : Entity_Id;
      Id : Attribute_Id) return Node_Id
   is
      N : Node_Id;

   begin
      N := First_Rep_Item (E);
      while Present (N) loop
         if Nkind (N) = N_Attribute_Definition_Clause
           and then Get_Attribute_Id (Chars (N)) = Id
         then
            return N;
         else
            Next_Rep_Item (N);
         end if;
      end loop;

      return Empty;
   end Get_Attribute_Definition_Clause;

   ---------------------------
   -- Get_Class_Wide_Pragma --
   ---------------------------

   function Get_Class_Wide_Pragma
     (E  : Entity_Id;
      Id : Pragma_Id) return Node_Id
    is
      Item  : Node_Id;
      Items : Node_Id;

   begin
      Items := Contract (E);

      if No (Items) then
         return Empty;
      end if;

      Item := Pre_Post_Conditions (Items);
      while Present (Item) loop
         if Nkind (Item) = N_Pragma
           and then Get_Pragma_Id (Pragma_Name_Unmapped (Item)) = Id
           and then Class_Present (Item)
         then
            return Item;
         end if;

         Item := Next_Pragma (Item);
      end loop;

      return Empty;
   end Get_Class_Wide_Pragma;

   -------------------
   -- Get_Full_View --
   -------------------

   function Get_Full_View (T : Entity_Id) return Entity_Id is
   begin
      if Is_Incomplete_Type (T) and then Present (Full_View (T)) then
         return Full_View (T);

      elsif Is_Class_Wide_Type (T)
        and then Is_Incomplete_Type (Root_Type (T))
        and then Present (Full_View (Root_Type (T)))
      then
         return Class_Wide_Type (Full_View (Root_Type (T)));

      else
         return T;
      end if;
   end Get_Full_View;

   ----------------
   -- Get_Pragma --
   ----------------

   function Get_Pragma (E : Entity_Id; Id : Pragma_Id) return Node_Id is

      --  Classification pragmas

      Is_CLS : constant Boolean :=
                 Id = Pragma_Abstract_State             or else
                 Id = Pragma_Attach_Handler             or else
                 Id = Pragma_Async_Readers              or else
                 Id = Pragma_Async_Writers              or else
                 Id = Pragma_Constant_After_Elaboration or else
                 Id = Pragma_Depends                    or else
                 Id = Pragma_Effective_Reads            or else
                 Id = Pragma_Effective_Writes           or else
                 Id = Pragma_Extensions_Visible         or else
                 Id = Pragma_Global                     or else
                 Id = Pragma_Initial_Condition          or else
                 Id = Pragma_Initializes                or else
                 Id = Pragma_Interrupt_Handler          or else
                 Id = Pragma_No_Caching                 or else
                 Id = Pragma_Part_Of                    or else
                 Id = Pragma_Refined_Depends            or else
                 Id = Pragma_Refined_Global             or else
                 Id = Pragma_Refined_State              or else
                 Id = Pragma_Volatile_Function;

      --  Contract / subprogram variant / test case pragmas

      Is_CTC : constant Boolean :=
                  Id = Pragma_Contract_Cases            or else
                  Id = Pragma_Subprogram_Variant        or else
                  Id = Pragma_Test_Case;

      --  Pre / postcondition pragmas

      Is_PPC : constant Boolean :=
                  Id = Pragma_Precondition              or else
                  Id = Pragma_Postcondition             or else
                  Id = Pragma_Refined_Post;

      In_Contract : constant Boolean := Is_CLS or Is_CTC or Is_PPC;

      Item  : Node_Id;
      Items : Node_Id;

   begin
      --  Handle pragmas that appear in N_Contract nodes. Those have to be
      --  extracted from their specialized list.

      if In_Contract then
         Items := Contract (E);

         if No (Items) then
            return Empty;

         elsif Is_CLS then
            Item := Classifications (Items);

         elsif Is_CTC then
            Item := Contract_Test_Cases (Items);

         else
            Item := Pre_Post_Conditions (Items);
         end if;

      --  Regular pragmas

      else
         Item := First_Rep_Item (E);
      end if;

      while Present (Item) loop
         if Nkind (Item) = N_Pragma
           and then Get_Pragma_Id (Pragma_Name_Unmapped (Item)) = Id
         then
            return Item;

         --  All nodes in N_Contract are chained using Next_Pragma

         elsif In_Contract then
            Item := Next_Pragma (Item);

         --  Regular pragmas

         else
            Next_Rep_Item (Item);
         end if;
      end loop;

      return Empty;
   end Get_Pragma;

   --------------------------------------
   -- Get_Record_Representation_Clause --
   --------------------------------------

   function Get_Record_Representation_Clause (E : Entity_Id) return Node_Id is
      N : Node_Id;

   begin
      N := First_Rep_Item (E);
      while Present (N) loop
         if Nkind (N) = N_Record_Representation_Clause then
            return N;
         end if;

         Next_Rep_Item (N);
      end loop;

      return Empty;
   end Get_Record_Representation_Clause;

   ------------------------
   -- Has_Attach_Handler --
   ------------------------

   function Has_Attach_Handler (Id : E) return B is
      Ritem : Node_Id;

   begin
      pragma Assert (Is_Protected_Type (Id));

      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Pragma
           and then Pragma_Name (Ritem) = Name_Attach_Handler
         then
            return True;
         else
            Next_Rep_Item (Ritem);
         end if;
      end loop;

      return False;
   end Has_Attach_Handler;

   -------------
   -- Has_DIC --
   -------------

   function Has_DIC (Id : E) return B is
   begin
      return Has_Own_DIC (Id) or else Has_Inherited_DIC (Id);
   end Has_DIC;

   -----------------
   -- Has_Entries --
   -----------------

   function Has_Entries (Id : E) return B is
      Ent : Entity_Id;

   begin
      pragma Assert (Is_Concurrent_Type (Id));

      Ent := First_Entity (Id);
      while Present (Ent) loop
         if Is_Entry (Ent) then
            return True;
         end if;

         Next_Entity (Ent);
      end loop;

      return False;
   end Has_Entries;

   ----------------------------
   -- Has_Foreign_Convention --
   ----------------------------

   function Has_Foreign_Convention (Id : E) return B is
   begin
      --  While regular Intrinsics such as the Standard operators fit in the
      --  "Ada" convention, those with an Interface_Name materialize GCC
      --  builtin imports for which Ada special treatments shouldn't apply.

      return Convention (Id) in Foreign_Convention
        or else (Convention (Id) = Convention_Intrinsic
                   and then Present (Interface_Name (Id)));
   end Has_Foreign_Convention;

   ---------------------------
   -- Has_Interrupt_Handler --
   ---------------------------

   function Has_Interrupt_Handler (Id : E) return B is
      Ritem : Node_Id;

   begin
      pragma Assert (Is_Protected_Type (Id));

      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Pragma
           and then Pragma_Name (Ritem) = Name_Interrupt_Handler
         then
            return True;
         else
            Next_Rep_Item (Ritem);
         end if;
      end loop;

      return False;
   end Has_Interrupt_Handler;

   --------------------
   -- Has_Invariants --
   --------------------

   function Has_Invariants (Id : E) return B is
   begin
      return Has_Own_Invariants (Id) or else Has_Inherited_Invariants (Id);
   end Has_Invariants;

   --------------------------
   -- Has_Limited_View --
   --------------------------

   function Has_Limited_View (Id : E) return B is
   begin
      return Ekind (Id) = E_Package
        and then not Is_Generic_Instance (Id)
        and then Present (Limited_View (Id));
   end Has_Limited_View;

   --------------------------
   -- Has_Non_Limited_View --
   --------------------------

   function Has_Non_Limited_View (Id : E) return B is
   begin
      return (Ekind (Id) in Incomplete_Kind
               or else Ekind (Id) in Class_Wide_Kind
               or else Ekind (Id) = E_Abstract_State)
        and then Present (Non_Limited_View (Id));
   end Has_Non_Limited_View;

   ---------------------------------
   -- Has_Non_Null_Abstract_State --
   ---------------------------------

   function Has_Non_Null_Abstract_State (Id : E) return B is
   begin
      pragma Assert (Is_Package_Or_Generic_Package (Id));

      return
        Present (Abstract_States (Id))
          and then
            not Is_Null_State (Node (First_Elmt (Abstract_States (Id))));
   end Has_Non_Null_Abstract_State;

   -------------------------------------
   -- Has_Non_Null_Visible_Refinement --
   -------------------------------------

   function Has_Non_Null_Visible_Refinement (Id : E) return B is
      Constits : Elist_Id;

   begin
      --  "Refinement" is a concept applicable only to abstract states

      pragma Assert (Ekind (Id) = E_Abstract_State);
      Constits := Refinement_Constituents (Id);

      --  A partial refinement is always non-null. For a full refinement to be
      --  non-null, the first constituent must be anything other than null.

      return
        Has_Partial_Visible_Refinement (Id)
          or else (Has_Visible_Refinement (Id)
                    and then Present (Constits)
                    and then Nkind (Node (First_Elmt (Constits))) /= N_Null);
   end Has_Non_Null_Visible_Refinement;

   -----------------------------
   -- Has_Null_Abstract_State --
   -----------------------------

   function Has_Null_Abstract_State (Id : E) return B is
      pragma Assert (Is_Package_Or_Generic_Package (Id));

      States : constant Elist_Id := Abstract_States (Id);

   begin
      --  Check first available state of related package. A null abstract
      --  state always appears as the sole element of the state list.

      return
        Present (States)
          and then Is_Null_State (Node (First_Elmt (States)));
   end Has_Null_Abstract_State;

   ---------------------------------
   -- Has_Null_Visible_Refinement --
   ---------------------------------

   function Has_Null_Visible_Refinement (Id : E) return B is
      Constits : Elist_Id;

   begin
      --  "Refinement" is a concept applicable only to abstract states

      pragma Assert (Ekind (Id) = E_Abstract_State);
      Constits := Refinement_Constituents (Id);

      --  For a refinement to be null, the state's sole constituent must be a
      --  null.

      return
        Has_Visible_Refinement (Id)
          and then Present (Constits)
          and then Nkind (Node (First_Elmt (Constits))) = N_Null;
   end Has_Null_Visible_Refinement;

   --------------------
   -- Has_Unmodified --
   --------------------

   function Has_Unmodified (E : Entity_Id) return Boolean is
   begin
      if Has_Pragma_Unmodified (E) then
         return True;
      elsif Warnings_Off (E) then
         Set_Warnings_Off_Used_Unmodified (E);
         return True;
      else
         return False;
      end if;
   end Has_Unmodified;

   ---------------------
   -- Has_Unreferenced --
   ---------------------

   function Has_Unreferenced (E : Entity_Id) return Boolean is
   begin
      if Has_Pragma_Unreferenced (E) then
         return True;
      elsif Warnings_Off (E) then
         Set_Warnings_Off_Used_Unreferenced (E);
         return True;
      else
         return False;
      end if;
   end Has_Unreferenced;

   ----------------------
   -- Has_Warnings_Off --
   ----------------------

   function Has_Warnings_Off (E : Entity_Id) return Boolean is
   begin
      if Warnings_Off (E) then
         Set_Warnings_Off_Used (E);
         return True;
      else
         return False;
      end if;
   end Has_Warnings_Off;

   ------------------------------
   -- Implementation_Base_Type --
   ------------------------------

   function Implementation_Base_Type (Id : E) return E is
      Bastyp : Entity_Id;
      Imptyp : Entity_Id;

   begin
      Bastyp := Base_Type (Id);

      if Is_Incomplete_Or_Private_Type (Bastyp) then
         Imptyp := Underlying_Type (Bastyp);

         --  If we have an implementation type, then just return it,
         --  otherwise we return the Base_Type anyway. This can only
         --  happen in error situations and should avoid some error bombs.

         if Present (Imptyp) then
            return Base_Type (Imptyp);
         else
            return Bastyp;
         end if;

      else
         return Bastyp;
      end if;
   end Implementation_Base_Type;

   -------------------------
   -- Invariant_Procedure --
   -------------------------

   function Invariant_Procedure (Id : E) return E is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id));

      Subps := Subprograms_For_Type (Base_Type (Id));

      if Present (Subps) then
         Subp_Elmt := First_Elmt (Subps);
         while Present (Subp_Elmt) loop
            Subp_Id := Node (Subp_Elmt);

            if Is_Invariant_Procedure (Subp_Id) then
               return Subp_Id;
            end if;

            Next_Elmt (Subp_Elmt);
         end loop;
      end if;

      return Empty;
   end Invariant_Procedure;

   ------------------
   -- Is_Base_Type --
   ------------------

   --  Global flag table allowing rapid computation of this function

   Entity_Is_Base_Type : constant array (Entity_Kind) of Boolean :=
     (E_Enumeration_Subtype          |
      E_Incomplete_Subtype           |
      E_Signed_Integer_Subtype       |
      E_Modular_Integer_Subtype      |
      E_Floating_Point_Subtype       |
      E_Ordinary_Fixed_Point_Subtype |
      E_Decimal_Fixed_Point_Subtype  |
      E_Array_Subtype                |
      E_Record_Subtype               |
      E_Private_Subtype              |
      E_Record_Subtype_With_Private  |
      E_Limited_Private_Subtype      |
      E_Access_Subtype               |
      E_Protected_Subtype            |
      E_Task_Subtype                 |
      E_String_Literal_Subtype       |
      E_Class_Wide_Subtype           => False,
      others                         => True);

   function Is_Base_Type (Id : E) return Boolean is
   begin
--  ????      pragma Assert (Is_Type (Id));
--  Apparently, Is_Base_Type is called on non-types, and returns True!
      return Entity_Is_Base_Type (Ekind (Id));
   end Is_Base_Type;

   ---------------------
   -- Is_Boolean_Type --
   ---------------------

   function Is_Boolean_Type (Id : E) return B is
   begin
      return Root_Type (Id) = Standard_Boolean;
   end Is_Boolean_Type;

   ------------------------
   -- Is_Constant_Object --
   ------------------------

   function Is_Constant_Object (Id : E) return B is
   begin
      return Ekind (Id) in E_Constant | E_In_Parameter | E_Loop_Parameter;
   end Is_Constant_Object;

   -------------------
   -- Is_Controlled --
   -------------------

   function Is_Controlled (Id : E) return B is
   begin
      return Is_Controlled_Active (Id) and then not Disable_Controlled (Id);
   end Is_Controlled;

   --------------------
   -- Is_Discriminal --
   --------------------

   function Is_Discriminal (Id : E) return B is
   begin
      return Ekind (Id) in E_Constant | E_In_Parameter
               and then Present (Discriminal_Link (Id));
   end Is_Discriminal;

   ----------------------
   -- Is_Dynamic_Scope --
   ----------------------

   function Is_Dynamic_Scope (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Block
          or else
        Ekind (Id) = E_Function
          or else
        Ekind (Id) = E_Procedure
          or else
        Ekind (Id) = E_Subprogram_Body
          or else
        Ekind (Id) = E_Task_Type
          or else
       (Ekind (Id) = E_Limited_Private_Type
         and then Present (Full_View (Id))
         and then Ekind (Full_View (Id)) = E_Task_Type)
          or else
        Ekind (Id) = E_Entry
          or else
        Ekind (Id) = E_Entry_Family
          or else
        Ekind (Id) = E_Return_Statement;
   end Is_Dynamic_Scope;

   --------------------
   -- Is_Entity_Name --
   --------------------

   function Is_Entity_Name (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      --  Identifiers, operator symbols, expanded names are entity names

      return Kind = N_Identifier
        or else Kind = N_Operator_Symbol
        or else Kind = N_Expanded_Name

      --  Attribute references are entity names if they refer to an entity.
      --  Note that we don't do this by testing for the presence of the
      --  Entity field in the N_Attribute_Reference node, since it may not
      --  have been set yet.

        or else (Kind = N_Attribute_Reference
                  and then Is_Entity_Attribute_Name (Attribute_Name (N)));
   end Is_Entity_Name;

   ---------------------------
   -- Is_Elaboration_Target --
   ---------------------------

   function Is_Elaboration_Target (Id : Entity_Id) return Boolean is
   begin
      return
        Ekind (Id) in E_Constant | E_Package | E_Variable
          or else Is_Entry        (Id)
          or else Is_Generic_Unit (Id)
          or else Is_Subprogram   (Id)
          or else Is_Task_Type    (Id);
   end Is_Elaboration_Target;

   -----------------------
   -- Is_External_State --
   -----------------------

   function Is_External_State (Id : E) return B is
   begin
      --  To qualify, the abstract state must appear with option "external" or
      --  "synchronous" (SPARK RM 7.1.4(7) and (9)).

      return
        Ekind (Id) = E_Abstract_State
          and then (Has_Option (Id, Name_External)
                      or else
                    Has_Option (Id, Name_Synchronous));
   end Is_External_State;

   ------------------
   -- Is_Finalizer --
   ------------------

   function Is_Finalizer (Id : E) return B is
   begin
      return Ekind (Id) = E_Procedure and then Chars (Id) = Name_uFinalizer;
   end Is_Finalizer;

   ----------------------
   -- Is_Full_Access --
   ----------------------

   function Is_Full_Access (Id : E) return B is
   begin
      return Is_Atomic (Id) or else Is_Volatile_Full_Access (Id);
   end Is_Full_Access;

   -------------------
   -- Is_Null_State --
   -------------------

   function Is_Null_State (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Abstract_State and then Nkind (Parent (Id)) = N_Null;
   end Is_Null_State;

   -----------------------------------
   -- Is_Package_Or_Generic_Package --
   -----------------------------------

   function Is_Package_Or_Generic_Package (Id : E) return B is
   begin
      return Ekind (Id) in E_Generic_Package | E_Package;
   end Is_Package_Or_Generic_Package;

   ---------------------
   -- Is_Packed_Array --
   ---------------------

   function Is_Packed_Array (Id : E) return B is
   begin
      return Is_Array_Type (Id) and then Is_Packed (Id);
   end Is_Packed_Array;

   ---------------
   -- Is_Prival --
   ---------------

   function Is_Prival (Id : E) return B is
   begin
      return Ekind (Id) in E_Constant | E_Variable
               and then Present (Prival_Link (Id));
   end Is_Prival;

   ----------------------------
   -- Is_Protected_Component --
   ----------------------------

   function Is_Protected_Component (Id : E) return B is
   begin
      return Ekind (Id) = E_Component and then Is_Protected_Type (Scope (Id));
   end Is_Protected_Component;

   ----------------------------
   -- Is_Protected_Interface --
   ----------------------------

   function Is_Protected_Interface (Id : E) return B is
      Typ : constant Entity_Id := Base_Type (Id);
   begin
      if not Is_Interface (Typ) then
         return False;
      elsif Is_Class_Wide_Type (Typ) then
         return Is_Protected_Interface (Etype (Typ));
      else
         return Protected_Present (Type_Definition (Parent (Typ)));
      end if;
   end Is_Protected_Interface;

   ------------------------------
   -- Is_Protected_Record_Type --
   ------------------------------

   function Is_Protected_Record_Type (Id : E) return B is
   begin
      return
        Is_Concurrent_Record_Type (Id)
          and then Is_Protected_Type (Corresponding_Concurrent_Type (Id));
   end Is_Protected_Record_Type;

   -------------------------------------
   -- Is_Relaxed_Initialization_State --
   -------------------------------------

   function Is_Relaxed_Initialization_State (Id : E) return B is
   begin
      --  To qualify, the abstract state must appear with simple option
      --  "Relaxed_Initialization" (SPARK RM 6.10).

      return
        Ekind (Id) = E_Abstract_State
          and then Has_Option (Id, Name_Relaxed_Initialization);
   end Is_Relaxed_Initialization_State;

   --------------------------------
   -- Is_Standard_Character_Type --
   --------------------------------

   function Is_Standard_Character_Type (Id : E) return B is
   begin
      return Is_Type (Id)
        and then Root_Type (Id) in Standard_Character
                                 | Standard_Wide_Character
                                 | Standard_Wide_Wide_Character;
   end Is_Standard_Character_Type;

   -----------------------------
   -- Is_Standard_String_Type --
   -----------------------------

   function Is_Standard_String_Type (Id : E) return B is
   begin
      return Is_Type (Id)
        and then Root_Type (Id) in Standard_String
                                 | Standard_Wide_String
                                 | Standard_Wide_Wide_String;
   end Is_Standard_String_Type;

   --------------------
   -- Is_String_Type --
   --------------------

   function Is_String_Type (Id : E) return B is
   begin
      return Is_Array_Type (Id)
        and then Id /= Any_Composite
        and then Number_Dimensions (Id) = 1
        and then Is_Character_Type (Component_Type (Id));
   end Is_String_Type;

   -------------------------------
   -- Is_Synchronized_Interface --
   -------------------------------

   function Is_Synchronized_Interface (Id : E) return B is
      Typ : constant Entity_Id := Base_Type (Id);

   begin
      if not Is_Interface (Typ) then
         return False;

      elsif Is_Class_Wide_Type (Typ) then
         return Is_Synchronized_Interface (Etype (Typ));

      else
         return    Protected_Present    (Type_Definition (Parent (Typ)))
           or else Synchronized_Present (Type_Definition (Parent (Typ)))
           or else Task_Present         (Type_Definition (Parent (Typ)));
      end if;
   end Is_Synchronized_Interface;

   ---------------------------
   -- Is_Synchronized_State --
   ---------------------------

   function Is_Synchronized_State (Id : E) return B is
   begin
      --  To qualify, the abstract state must appear with simple option
      --  "synchronous" (SPARK RM 7.1.4(9)).

      return
        Ekind (Id) = E_Abstract_State
          and then Has_Option (Id, Name_Synchronous);
   end Is_Synchronized_State;

   -----------------------
   -- Is_Task_Interface --
   -----------------------

   function Is_Task_Interface (Id : E) return B is
      Typ : constant Entity_Id := Base_Type (Id);
   begin
      if not Is_Interface (Typ) then
         return False;
      elsif Is_Class_Wide_Type (Typ) then
         return Is_Task_Interface (Etype (Typ));
      else
         return Task_Present (Type_Definition (Parent (Typ)));
      end if;
   end Is_Task_Interface;

   -------------------------
   -- Is_Task_Record_Type --
   -------------------------

   function Is_Task_Record_Type (Id : E) return B is
   begin
      return
        Is_Concurrent_Record_Type (Id)
          and then Is_Task_Type (Corresponding_Concurrent_Type (Id));
   end Is_Task_Record_Type;

   ------------------------
   -- Is_Wrapper_Package --
   ------------------------

   function Is_Wrapper_Package (Id : E) return B is
   begin
      return Ekind (Id) = E_Package and then Present (Related_Instance (Id));
   end Is_Wrapper_Package;

   -----------------
   -- Last_Formal --
   -----------------

   function Last_Formal (Id : E) return E is
      Formal : Entity_Id;

   begin
      pragma Assert
        (Is_Overloadable (Id)
          or else Ekind (Id) in E_Entry_Family
                              | E_Subprogram_Body
                              | E_Subprogram_Type);

      if Ekind (Id) = E_Enumeration_Literal then
         return Empty;

      else
         Formal := First_Formal (Id);

         if Present (Formal) then
            while Present (Next_Formal (Formal)) loop
               Next_Formal (Formal);
            end loop;
         end if;

         return Formal;
      end if;
   end Last_Formal;

   -------------------
   -- Link_Entities --
   -------------------

   procedure Link_Entities (First : Entity_Id; Second : Node_Id) is
   begin
      if Present (Second) then
         Set_Prev_Entity (Second, First);  --  First <-- Second
      end if;

      Set_Next_Entity (First, Second);     --  First --> Second
   end Link_Entities;

   ------------------------
   -- Machine_Emax_Value --
   ------------------------

   function Machine_Emax_Value (Id : E) return Uint is
      Digs : constant Pos := UI_To_Int (Digits_Value (Base_Type (Id)));

   begin
      case Float_Rep (Id) is
         when IEEE_Binary =>
            case Digs is
               when  1 ..  6 => return Uint_128;
               when  7 .. 15 => return 2**10;
               when 16 .. 33 => return 2**14;
               when others   => return No_Uint;
            end case;

         when AAMP =>
            return Uint_2 ** Uint_7 - Uint_1;
      end case;
   end Machine_Emax_Value;

   ------------------------
   -- Machine_Emin_Value --
   ------------------------

   function Machine_Emin_Value (Id : E) return Uint is
   begin
      case Float_Rep (Id) is
         when IEEE_Binary => return Uint_3 - Machine_Emax_Value (Id);
         when AAMP        => return -Machine_Emax_Value (Id);
      end case;
   end Machine_Emin_Value;

   ----------------------------
   -- Machine_Mantissa_Value --
   ----------------------------

   function Machine_Mantissa_Value (Id : E) return Uint is
      Digs : constant Pos := UI_To_Int (Digits_Value (Base_Type (Id)));

   begin
      case Float_Rep (Id) is
         when IEEE_Binary =>
            case Digs is
               when  1 ..  6 => return Uint_24;
               when  7 .. 15 => return UI_From_Int (53);
               when 16 .. 18 => return Uint_64;
               when 19 .. 33 => return UI_From_Int (113);
               when others   => return No_Uint;
            end case;

         when AAMP =>
            case Digs is
               when  1 ..  6 => return Uint_24;
               when  7 ..  9 => return UI_From_Int (40);
               when others   => return No_Uint;
            end case;
      end case;
   end Machine_Mantissa_Value;

   -------------------------
   -- Machine_Radix_Value --
   -------------------------

   function Machine_Radix_Value (Id : E) return U is
   begin
      case Float_Rep (Id) is
         when AAMP
            | IEEE_Binary
         =>
            return Uint_2;
      end case;
   end Machine_Radix_Value;

   ----------------------
   -- Model_Emin_Value --
   ----------------------

   function Model_Emin_Value (Id : E) return Uint is
   begin
      return Machine_Emin_Value (Id);
   end Model_Emin_Value;

   -------------------------
   -- Model_Epsilon_Value --
   -------------------------

   function Model_Epsilon_Value (Id : E) return Ureal is
      Radix : constant Ureal := UR_From_Uint (Machine_Radix_Value (Id));
   begin
      return Radix ** (1 - Model_Mantissa_Value (Id));
   end Model_Epsilon_Value;

   --------------------------
   -- Model_Mantissa_Value --
   --------------------------

   function Model_Mantissa_Value (Id : E) return Uint is
   begin
      return Machine_Mantissa_Value (Id);
   end Model_Mantissa_Value;

   -----------------------
   -- Model_Small_Value --
   -----------------------

   function Model_Small_Value (Id : E) return Ureal is
      Radix : constant Ureal := UR_From_Uint (Machine_Radix_Value (Id));
   begin
      return Radix ** (Model_Emin_Value (Id) - 1);
   end Model_Small_Value;

   --------------------
   -- Next_Component --
   --------------------

   function Next_Component (Id : E) return E is
      Comp_Id : Entity_Id;

   begin
      Comp_Id := Next_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) = E_Component;
         Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end Next_Component;

   ------------------------------------
   -- Next_Component_Or_Discriminant --
   ------------------------------------

   function Next_Component_Or_Discriminant (Id : E) return E is
      Comp_Id : Entity_Id;

   begin
      Comp_Id := Next_Entity (Id);
      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) in E_Component | E_Discriminant;
         Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end Next_Component_Or_Discriminant;

   -----------------------
   -- Next_Discriminant --
   -----------------------

   --  This function actually implements both Next_Discriminant and
   --  Next_Stored_Discriminant by making sure that the Discriminant
   --  returned is of the same variety as Id.

   function Next_Discriminant (Id : E) return E is

      --  Derived Tagged types with private extensions look like this...

      --       E_Discriminant d1
      --       E_Discriminant d2
      --       E_Component    _tag
      --       E_Discriminant d1
      --       E_Discriminant d2
      --       ...

      --  so it is critical not to go past the leading discriminants

      D : E := Id;

   begin
      pragma Assert (Ekind (Id) = E_Discriminant);

      loop
         Next_Entity (D);
         if No (D)
           or else (Ekind (D) /= E_Discriminant
                      and then not Is_Itype (D))
         then
            return Empty;
         end if;

         exit when Ekind (D) = E_Discriminant
           and then (Is_Completely_Hidden (D) = Is_Completely_Hidden (Id));
      end loop;

      return D;
   end Next_Discriminant;

   -----------------
   -- Next_Formal --
   -----------------

   function Next_Formal (Id : E) return E is
      P : Entity_Id;

   begin
      --  Follow the chain of declared entities as long as the kind of the
      --  entity corresponds to a formal parameter. Skip internal entities
      --  that may have been created for implicit subtypes, in the process
      --  of analyzing default expressions.

      P := Id;
      loop
         Next_Entity (P);

         if No (P) or else Is_Formal (P) then
            return P;
         elsif not Is_Internal (P) then
            return Empty;
         end if;
      end loop;
   end Next_Formal;

   -----------------------------
   -- Next_Formal_With_Extras --
   -----------------------------

   function Next_Formal_With_Extras (Id : E) return E is
   begin
      if Present (Extra_Formal (Id)) then
         return Extra_Formal (Id);
      else
         return Next_Formal (Id);
      end if;
   end Next_Formal_With_Extras;

   ----------------
   -- Next_Index --
   ----------------

   function Next_Index (Id : Node_Id) return Node_Id is
   begin
      return Next (Id);
   end Next_Index;

   ------------------
   -- Next_Literal --
   ------------------

   function Next_Literal (Id : E) return E is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Next (Id);
   end Next_Literal;

   ------------------------------
   -- Next_Stored_Discriminant --
   ------------------------------

   function Next_Stored_Discriminant (Id : E) return E is
   begin
      --  See comment in Next_Discriminant

      return Next_Discriminant (Id);
   end Next_Stored_Discriminant;

   -----------------------
   -- Number_Dimensions --
   -----------------------

   function Number_Dimensions (Id : E) return Pos is
      N : Int;
      T : Node_Id;

   begin
      if Ekind (Id) = E_String_Literal_Subtype then
         return 1;

      else
         N := 0;
         T := First_Index (Id);
         while Present (T) loop
            N := N + 1;
            Next_Index (T);
         end loop;

         return N;
      end if;
   end Number_Dimensions;

   --------------------
   -- Number_Entries --
   --------------------

   function Number_Entries (Id : E) return Nat is
      N   : Int;
      Ent : Entity_Id;

   begin
      pragma Assert (Is_Concurrent_Type (Id));

      N := 0;
      Ent := First_Entity (Id);
      while Present (Ent) loop
         if Is_Entry (Ent) then
            N := N + 1;
         end if;

         Next_Entity (Ent);
      end loop;

      return N;
   end Number_Entries;

   --------------------
   -- Number_Formals --
   --------------------

   function Number_Formals (Id : E) return Pos is
      N      : Int;
      Formal : Entity_Id;

   begin
      N := 0;
      Formal := First_Formal (Id);
      while Present (Formal) loop
         N := N + 1;
         Next_Formal (Formal);
      end loop;

      return N;
   end Number_Formals;

   ------------------------
   -- Object_Size_Clause --
   ------------------------

   function Object_Size_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Object_Size);
   end Object_Size_Clause;

   --------------------
   -- Parameter_Mode --
   --------------------

   function Parameter_Mode (Id : E) return Formal_Kind is
   begin
      return Ekind (Id);
   end Parameter_Mode;

   -------------------
   -- DIC_Procedure --
   -------------------

   function DIC_Procedure (Id : E) return E is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id));

      Subps := Subprograms_For_Type (Base_Type (Id));

      if Present (Subps) then
         Subp_Elmt := First_Elmt (Subps);
         while Present (Subp_Elmt) loop
            Subp_Id := Node (Subp_Elmt);

            --  Currently the flag Is_DIC_Procedure is set for both normal DIC
            --  check procedures as well as for partial DIC check procedures,
            --  and we don't have a flag for the partial procedures.

            if Is_DIC_Procedure (Subp_Id)
              and then not Is_Partial_DIC_Procedure (Subp_Id)
            then
               return Subp_Id;
            end if;

            Next_Elmt (Subp_Elmt);
         end loop;
      end if;

      return Empty;
   end DIC_Procedure;

   function Partial_DIC_Procedure (Id : E) return E is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id));

      Subps := Subprograms_For_Type (Base_Type (Id));

      if Present (Subps) then
         Subp_Elmt := First_Elmt (Subps);
         while Present (Subp_Elmt) loop
            Subp_Id := Node (Subp_Elmt);

            if Is_Partial_DIC_Procedure (Subp_Id) then
               return Subp_Id;
            end if;

            Next_Elmt (Subp_Elmt);
         end loop;
      end if;

      return Empty;
   end Partial_DIC_Procedure;

   function Is_Partial_DIC_Procedure (Id : E) return B is
      Partial_DIC_Suffix : constant String := "Partial_DIC";
      DIC_Nam            : constant String := Get_Name_String (Chars (Id));

   begin
      pragma Assert (Ekind (Id) in E_Function | E_Procedure);

      --  Instead of adding a new Entity_Id flag (which are in short supply),
      --  we test the form of the subprogram name. When the node field and flag
      --  situation is eased, this should be replaced with a flag. ???

      if DIC_Nam'Length > Partial_DIC_Suffix'Length
        and then
          DIC_Nam
            (DIC_Nam'Last - Partial_DIC_Suffix'Length + 1 .. DIC_Nam'Last) =
               Partial_DIC_Suffix
      then
         return True;
      else
         return False;
      end if;
   end Is_Partial_DIC_Procedure;

   ---------------------------------
   -- Partial_Invariant_Procedure --
   ---------------------------------

   function Partial_Invariant_Procedure (Id : E) return E is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id));

      Subps := Subprograms_For_Type (Base_Type (Id));

      if Present (Subps) then
         Subp_Elmt := First_Elmt (Subps);
         while Present (Subp_Elmt) loop
            Subp_Id := Node (Subp_Elmt);

            if Is_Partial_Invariant_Procedure (Subp_Id) then
               return Subp_Id;
            end if;

            Next_Elmt (Subp_Elmt);
         end loop;
      end if;

      return Empty;
   end Partial_Invariant_Procedure;

   -------------------------------------
   -- Partial_Refinement_Constituents --
   -------------------------------------

   function Partial_Refinement_Constituents (Id : E) return L is
      Constits : Elist_Id := No_Elist;

      procedure Add_Usable_Constituents (Item : E);
      --  Add global item Item and/or its constituents to list Constits when
      --  they can be used in a global refinement within the current scope. The
      --  criteria are:
      --    1) If Item is an abstract state with full refinement visible, add
      --       its constituents.
      --    2) If Item is an abstract state with only partial refinement
      --       visible, add both Item and its constituents.
      --    3) If Item is an abstract state without a visible refinement, add
      --       it.
      --    4) If Id is not an abstract state, add it.

      procedure Add_Usable_Constituents (List : Elist_Id);
      --  Apply Add_Usable_Constituents to every constituent in List

      -----------------------------
      -- Add_Usable_Constituents --
      -----------------------------

      procedure Add_Usable_Constituents (Item : E) is
      begin
         if Ekind (Item) = E_Abstract_State then
            if Has_Visible_Refinement (Item) then
               Add_Usable_Constituents (Refinement_Constituents (Item));

            elsif Has_Partial_Visible_Refinement (Item) then
               Append_New_Elmt (Item, Constits);
               Add_Usable_Constituents (Part_Of_Constituents (Item));

            else
               Append_New_Elmt (Item, Constits);
            end if;

         else
            Append_New_Elmt (Item, Constits);
         end if;
      end Add_Usable_Constituents;

      procedure Add_Usable_Constituents (List : Elist_Id) is
         Constit_Elmt : Elmt_Id;
      begin
         if Present (List) then
            Constit_Elmt := First_Elmt (List);
            while Present (Constit_Elmt) loop
               Add_Usable_Constituents (Node (Constit_Elmt));
               Next_Elmt (Constit_Elmt);
            end loop;
         end if;
      end Add_Usable_Constituents;

   --  Start of processing for Partial_Refinement_Constituents

   begin
      --  "Refinement" is a concept applicable only to abstract states

      pragma Assert (Ekind (Id) = E_Abstract_State);

      if Has_Visible_Refinement (Id) then
         Constits := Refinement_Constituents (Id);

      --  A refinement may be partially visible when objects declared in the
      --  private part of a package are subject to a Part_Of indicator.

      elsif Has_Partial_Visible_Refinement (Id) then
         Add_Usable_Constituents (Part_Of_Constituents (Id));

      --  Function should only be called when full or partial refinement is
      --  visible.

      else
         raise Program_Error;
      end if;

      return Constits;
   end Partial_Refinement_Constituents;

   ------------------------
   -- Predicate_Function --
   ------------------------

   function Predicate_Function (Id : E) return E is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;
      Typ       : Entity_Id;

   begin
      pragma Assert (Is_Type (Id));

      --  If type is private and has a completion, predicate may be defined on
      --  the full view.

      if Is_Private_Type (Id)
         and then
           (not Has_Predicates (Id) or else No (Subprograms_For_Type (Id)))
         and then Present (Full_View (Id))
      then
         Typ := Full_View (Id);

      elsif Ekind (Id) in E_Array_Subtype
                        | E_Record_Subtype
                        | E_Record_Subtype_With_Private
        and then Present (Predicated_Parent (Id))
      then
         Typ := Predicated_Parent (Id);

      else
         Typ := Id;
      end if;

      Subps := Subprograms_For_Type (Typ);

      if Present (Subps) then
         Subp_Elmt := First_Elmt (Subps);
         while Present (Subp_Elmt) loop
            Subp_Id := Node (Subp_Elmt);

            if Ekind (Subp_Id) = E_Function
              and then Is_Predicate_Function (Subp_Id)
            then
               return Subp_Id;
            end if;

            Next_Elmt (Subp_Elmt);
         end loop;
      end if;

      return Empty;
   end Predicate_Function;

   --------------------------
   -- Predicate_Function_M --
   --------------------------

   function Predicate_Function_M (Id : E) return E is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;
      Typ       : Entity_Id;

   begin
      pragma Assert (Is_Type (Id));

      --  If type is private and has a completion, predicate may be defined on
      --  the full view.

      if Is_Private_Type (Id)
         and then
           (not Has_Predicates (Id) or else No (Subprograms_For_Type (Id)))
         and then Present (Full_View (Id))
      then
         Typ := Full_View (Id);

      else
         Typ := Id;
      end if;

      Subps := Subprograms_For_Type (Typ);

      if Present (Subps) then
         Subp_Elmt := First_Elmt (Subps);
         while Present (Subp_Elmt) loop
            Subp_Id := Node (Subp_Elmt);

            if Ekind (Subp_Id) = E_Function
              and then Is_Predicate_Function_M (Subp_Id)
            then
               return Subp_Id;
            end if;

            Next_Elmt (Subp_Elmt);
         end loop;
      end if;

      return Empty;
   end Predicate_Function_M;

   -------------------------
   -- Present_In_Rep_Item --
   -------------------------

   function Present_In_Rep_Item (E : Entity_Id; N : Node_Id) return Boolean is
      Ritem : Node_Id;

   begin
      Ritem := First_Rep_Item (E);

      while Present (Ritem) loop
         if Ritem = N then
            return True;
         end if;

         Next_Rep_Item (Ritem);
      end loop;

      return False;
   end Present_In_Rep_Item;

   --------------------------
   -- Primitive_Operations --
   --------------------------

   function Primitive_Operations (Id : E) return L is
   begin
      if Is_Concurrent_Type (Id) then
         if Present (Corresponding_Record_Type (Id)) then
            return Direct_Primitive_Operations
              (Corresponding_Record_Type (Id));

         --  If expansion is disabled the corresponding record type is absent,
         --  but if the type has ancestors it may have primitive operations.

         elsif Is_Tagged_Type (Id) then
            return Direct_Primitive_Operations (Id);

         else
            return No_Elist;
         end if;
      else
         return Direct_Primitive_Operations (Id);
      end if;
   end Primitive_Operations;

   ---------------------
   -- Record_Rep_Item --
   ---------------------

   procedure Record_Rep_Item (E : Entity_Id; N : Node_Id) is
   begin
      Set_Next_Rep_Item (N, First_Rep_Item (E));
      Set_First_Rep_Item (E, N);
   end Record_Rep_Item;

   -------------------
   -- Remove_Entity --
   -------------------

   procedure Remove_Entity (Id : Entity_Id) is
      Next  : constant Entity_Id := Next_Entity (Id);
      Prev  : constant Entity_Id := Prev_Entity (Id);
      Scop  : constant Entity_Id := Scope (Id);
      First : constant Entity_Id := First_Entity (Scop);
      Last  : constant Entity_Id := Last_Entity  (Scop);

   begin
      --  Eliminate any existing linkages from the entity

      Set_Prev_Entity (Id, Empty);  --  Empty <-- Id
      Set_Next_Entity (Id, Empty);  --  Id --> Empty

      --  The eliminated entity was the only element in the entity chain

      if Id = First and then Id = Last then
         Set_First_Entity (Scop, Empty);
         Set_Last_Entity  (Scop, Empty);

      --  The eliminated entity was the head of the entity chain

      elsif Id = First then
         Set_First_Entity (Scop, Next);

      --  The eliminated entity was the tail of the entity chain

      elsif Id = Last then
         Set_Last_Entity (Scop, Prev);

      --  Otherwise the eliminated entity comes from the middle of the entity
      --  chain.

      else
         Link_Entities (Prev, Next);  --  Prev <-- Next, Prev --> Next
      end if;
   end Remove_Entity;

   ---------------
   -- Root_Type --
   ---------------

   function Root_Type (Id : E) return E is
      T, Etyp : Entity_Id;

   begin
      pragma Assert (Nkind (Id) in N_Entity);

      T := Base_Type (Id);

      if Ekind (T) = E_Class_Wide_Type then
         return Etype (T);

      --  Other cases

      else
         loop
            Etyp := Etype (T);

            if T = Etyp then
               return T;

            --  Following test catches some error cases resulting from
            --  previous errors.

            elsif No (Etyp) then
               Check_Error_Detected;
               return T;

            elsif Is_Private_Type (T) and then Etyp = Full_View (T) then
               return T;

            elsif Is_Private_Type (Etyp) and then Full_View (Etyp) = T then
               return T;
            end if;

            T := Etyp;

            --  Return if there is a circularity in the inheritance chain. This
            --  happens in some error situations and we do not want to get
            --  stuck in this loop.

            if T = Base_Type (Id) then
               return T;
            end if;
         end loop;
      end if;
   end Root_Type;

   ---------------------
   -- Safe_Emax_Value --
   ---------------------

   function Safe_Emax_Value (Id : E) return Uint is
   begin
      return Machine_Emax_Value (Id);
   end Safe_Emax_Value;

   ----------------------
   -- Safe_First_Value --
   ----------------------

   function Safe_First_Value (Id : E) return Ureal is
   begin
      return -Safe_Last_Value (Id);
   end Safe_First_Value;

   ---------------------
   -- Safe_Last_Value --
   ---------------------

   function Safe_Last_Value (Id : E) return Ureal is
      Radix       : constant Uint := Machine_Radix_Value (Id);
      Mantissa    : constant Uint := Machine_Mantissa_Value (Id);
      Emax        : constant Uint := Safe_Emax_Value (Id);
      Significand : constant Uint := Radix ** Mantissa - 1;
      Exponent    : constant Uint := Emax - Mantissa;

   begin
      if Radix = 2 then
         return
           UR_From_Components
             (Num   => Significand * 2 ** (Exponent mod 4),
              Den   => -Exponent / 4,
              Rbase => 16);
      else
         return
           UR_From_Components
             (Num => Significand,
              Den => -Exponent,
              Rbase => 16);
      end if;
   end Safe_Last_Value;

   -----------------
   -- Scope_Depth --
   -----------------

   function Scope_Depth (Id : E) return Uint is
      Scop : Entity_Id;

   begin
      Scop := Id;
      while Is_Record_Type (Scop) loop
         Scop := Scope (Scop);
      end loop;

      return Scope_Depth_Value (Scop);
   end Scope_Depth;

   ---------------------
   -- Scope_Depth_Set --
   ---------------------

   function Scope_Depth_Set (Id : E) return B is
   begin
      return not Is_Record_Type (Id)
        and then not Field_Is_Initial_Zero (Id, Scope_Depth_Value);
      --  We can't call Scope_Depth_Value here, because Empty is not a valid
      --  value of type Uint.
   end Scope_Depth_Set;

   --------------------
   -- Set_Convention --
   --------------------

   procedure Set_Convention (E : Entity_Id; Val : Snames.Convention_Id) is
   begin
      Set_Basic_Convention (E, Val);

      if Ekind (E) in Access_Subprogram_Kind
        and then Has_Foreign_Convention (E)
      then
         Set_Can_Use_Internal_Rep (E, False);
      end if;

      --  If E is an object, including a component, and the type of E is an
      --  anonymous access type with no convention set, then also set the
      --  convention of the anonymous access type. We do not do this for
      --  anonymous protected types, since protected types always have the
      --  default convention.

      if Present (Etype (E))
        and then (Is_Object (E)

                   --  Allow E_Void (happens for pragma Convention appearing
                   --  in the middle of a record applying to a component)

                   or else Ekind (E) = E_Void)
      then
         declare
            Typ : constant Entity_Id := Etype (E);

         begin
            if Ekind (Typ) in E_Anonymous_Access_Type
                            | E_Anonymous_Access_Subprogram_Type
              and then not Has_Convention_Pragma (Typ)
            then
               Set_Basic_Convention (Typ, Val);
               Set_Has_Convention_Pragma (Typ);

               --  And for the access subprogram type, deal similarly with the
               --  designated E_Subprogram_Type, which is always internal.

               if Ekind (Typ) = E_Anonymous_Access_Subprogram_Type then
                  declare
                     Dtype : constant Entity_Id := Designated_Type (Typ);
                  begin
                     if Ekind (Dtype) = E_Subprogram_Type
                       and then not Has_Convention_Pragma (Dtype)
                     then
                        Set_Basic_Convention (Dtype, Val);
                        Set_Has_Convention_Pragma (Dtype);
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
   end Set_Convention;

   -----------------------
   -- Set_DIC_Procedure --
   -----------------------

   procedure Set_DIC_Procedure (Id : E; V : E) is
      Base_Typ  : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id));

      Base_Typ := Base_Type (Id);
      Subps    := Subprograms_For_Type (Base_Typ);

      if No (Subps) then
         Subps := New_Elmt_List;
         Set_Subprograms_For_Type (Base_Typ, Subps);
      end if;

      Prepend_Elmt (V, Subps);
   end Set_DIC_Procedure;

   procedure Set_Partial_DIC_Procedure (Id : E; V : E) is
   begin
      Set_DIC_Procedure (Id, V);
   end Set_Partial_DIC_Procedure;

   -----------------------------
   -- Set_Invariant_Procedure --
   -----------------------------

   procedure Set_Invariant_Procedure (Id : E; V : E) is
      Base_Typ  : Entity_Id;
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id));

      Base_Typ := Base_Type (Id);
      Subps    := Subprograms_For_Type (Base_Typ);

      if No (Subps) then
         Subps := New_Elmt_List;
         Set_Subprograms_For_Type (Base_Typ, Subps);
      end if;

      Subp_Elmt := First_Elmt (Subps);
      Prepend_Elmt (V, Subps);

      --  Check for a duplicate invariant procedure

      while Present (Subp_Elmt) loop
         Subp_Id := Node (Subp_Elmt);

         if Is_Invariant_Procedure (Subp_Id) then
            raise Program_Error;
         end if;

         Next_Elmt (Subp_Elmt);
      end loop;
   end Set_Invariant_Procedure;

   -------------------------------------
   -- Set_Partial_Invariant_Procedure --
   -------------------------------------

   procedure Set_Partial_Invariant_Procedure (Id : E; V : E) is
      Base_Typ  : Entity_Id;
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id));

      Base_Typ := Base_Type (Id);
      Subps    := Subprograms_For_Type (Base_Typ);

      if No (Subps) then
         Subps := New_Elmt_List;
         Set_Subprograms_For_Type (Base_Typ, Subps);
      end if;

      Subp_Elmt := First_Elmt (Subps);
      Prepend_Elmt (V, Subps);

      --  Check for a duplicate partial invariant procedure

      while Present (Subp_Elmt) loop
         Subp_Id := Node (Subp_Elmt);

         if Is_Partial_Invariant_Procedure (Subp_Id) then
            raise Program_Error;
         end if;

         Next_Elmt (Subp_Elmt);
      end loop;
   end Set_Partial_Invariant_Procedure;

   ----------------------------
   -- Set_Predicate_Function --
   ----------------------------

   procedure Set_Predicate_Function (Id : E; V : E) is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id) and then Has_Predicates (Id));

      Subps := Subprograms_For_Type (Id);

      if No (Subps) then
         Subps := New_Elmt_List;
         Set_Subprograms_For_Type (Id, Subps);
      end if;

      Subp_Elmt := First_Elmt (Subps);
      Prepend_Elmt (V, Subps);

      --  Check for a duplicate predication function

      while Present (Subp_Elmt) loop
         Subp_Id := Node (Subp_Elmt);

         if Ekind (Subp_Id) = E_Function
           and then Is_Predicate_Function (Subp_Id)
         then
            raise Program_Error;
         end if;

         Next_Elmt (Subp_Elmt);
      end loop;
   end Set_Predicate_Function;

   ------------------------------
   -- Set_Predicate_Function_M --
   ------------------------------

   procedure Set_Predicate_Function_M (Id : E; V : E) is
      Subp_Elmt : Elmt_Id;
      Subp_Id   : Entity_Id;
      Subps     : Elist_Id;

   begin
      pragma Assert (Is_Type (Id) and then Has_Predicates (Id));

      Subps := Subprograms_For_Type (Id);

      if No (Subps) then
         Subps := New_Elmt_List;
         Set_Subprograms_For_Type (Id, Subps);
      end if;

      Subp_Elmt := First_Elmt (Subps);
      Prepend_Elmt (V, Subps);

      --  Check for a duplicate predication function

      while Present (Subp_Elmt) loop
         Subp_Id := Node (Subp_Elmt);

         if Ekind (Subp_Id) = E_Function
           and then Is_Predicate_Function_M (Subp_Id)
         then
            raise Program_Error;
         end if;

         Next_Elmt (Subp_Elmt);
      end loop;
   end Set_Predicate_Function_M;

   -----------------
   -- Size_Clause --
   -----------------

   function Size_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Size);
   end Size_Clause;

   ------------------------
   -- Stream_Size_Clause --
   ------------------------

   function Stream_Size_Clause (Id : E) return N is
   begin
      return Get_Attribute_Definition_Clause (Id, Attribute_Stream_Size);
   end Stream_Size_Clause;

   ------------------
   -- Subtype_Kind --
   ------------------

   function Subtype_Kind (K : Entity_Kind) return Entity_Kind is
      Kind : Entity_Kind;

   begin
      case K is
         when Access_Kind =>
            Kind := E_Access_Subtype;

         when E_Array_Subtype
            | E_Array_Type
         =>
            Kind := E_Array_Subtype;

         when E_Class_Wide_Subtype
            | E_Class_Wide_Type
         =>
            Kind := E_Class_Wide_Subtype;

         when E_Decimal_Fixed_Point_Subtype
            | E_Decimal_Fixed_Point_Type
         =>
            Kind := E_Decimal_Fixed_Point_Subtype;

         when E_Ordinary_Fixed_Point_Subtype
            | E_Ordinary_Fixed_Point_Type
         =>
            Kind := E_Ordinary_Fixed_Point_Subtype;

         when E_Private_Subtype
            | E_Private_Type
         =>
            Kind := E_Private_Subtype;

         when E_Limited_Private_Subtype
            | E_Limited_Private_Type
         =>
            Kind := E_Limited_Private_Subtype;

         when E_Record_Subtype_With_Private
            | E_Record_Type_With_Private
         =>
            Kind := E_Record_Subtype_With_Private;

         when E_Record_Subtype
            | E_Record_Type
         =>
            Kind := E_Record_Subtype;

         when Enumeration_Kind =>
            Kind := E_Enumeration_Subtype;

         when E_Incomplete_Type =>
            Kind := E_Incomplete_Subtype;

         when Float_Kind =>
            Kind := E_Floating_Point_Subtype;

         when Signed_Integer_Kind =>
            Kind := E_Signed_Integer_Subtype;

         when Modular_Integer_Kind =>
            Kind := E_Modular_Integer_Subtype;

         when Protected_Kind =>
            Kind := E_Protected_Subtype;

         when Task_Kind =>
            Kind := E_Task_Subtype;

         when others =>
            raise Program_Error;
      end case;

      return Kind;
   end Subtype_Kind;

   ---------------------
   -- Type_High_Bound --
   ---------------------

   function Type_High_Bound (Id : E) return Node_Id is
      Rng : constant Node_Id := Scalar_Range (Id);
   begin
      if Nkind (Rng) = N_Subtype_Indication then
         return High_Bound (Range_Expression (Constraint (Rng)));
      else
         return High_Bound (Rng);
      end if;
   end Type_High_Bound;

   --------------------
   -- Type_Low_Bound --
   --------------------

   function Type_Low_Bound (Id : E) return Node_Id is
      Rng : constant Node_Id := Scalar_Range (Id);
   begin
      if Nkind (Rng) = N_Subtype_Indication then
         return Low_Bound (Range_Expression (Constraint (Rng)));
      else
         return Low_Bound (Rng);
      end if;
   end Type_Low_Bound;

   ---------------------
   -- Underlying_Type --
   ---------------------

   function Underlying_Type (Id : E) return E is
   begin
      --  For record_with_private the underlying type is always the direct full
      --  view. Never try to take the full view of the parent it does not make
      --  sense.

      if Ekind (Id) = E_Record_Type_With_Private then
         return Full_View (Id);

      --  If we have a class-wide type that comes from the limited view then we
      --  return the Underlying_Type of its nonlimited view.

      elsif Ekind (Id) = E_Class_Wide_Type
        and then From_Limited_With (Id)
        and then Present (Non_Limited_View (Id))
      then
         return Underlying_Type (Non_Limited_View (Id));

      elsif Ekind (Id) in Incomplete_Or_Private_Kind then

         --  If we have an incomplete or private type with a full view, then we
         --  return the Underlying_Type of this full view.

         if Present (Full_View (Id)) then
            if Id = Full_View (Id) then

               --  Previous error in declaration

               return Empty;

            else
               return Underlying_Type (Full_View (Id));
            end if;

         --  If we have a private type with an underlying full view, then we
         --  return the Underlying_Type of this underlying full view.

         elsif Ekind (Id) in Private_Kind
           and then Present (Underlying_Full_View (Id))
         then
            return Underlying_Type (Underlying_Full_View (Id));

         --  If we have an incomplete entity that comes from the limited view
         --  then we return the Underlying_Type of its nonlimited view.

         elsif From_Limited_With (Id)
           and then Present (Non_Limited_View (Id))
         then
            return Underlying_Type (Non_Limited_View (Id));

         --  Otherwise check for the case where we have a derived type or
         --  subtype, and if so get the Underlying_Type of the parent type.

         elsif Etype (Id) /= Id then
            return Underlying_Type (Etype (Id));

         --  Otherwise we have an incomplete or private type that has no full
         --  view, which means that we have not encountered the completion, so
         --  return Empty to indicate the underlying type is not yet known.

         else
            return Empty;
         end if;

      --  For non-incomplete, non-private types, return the type itself. Also
      --  for entities that are not types at all return the entity itself.

      else
         return Id;
      end if;
   end Underlying_Type;

   ------------------------
   -- Unlink_Next_Entity --
   ------------------------

   procedure Unlink_Next_Entity (Id : Entity_Id) is
      Next : constant Entity_Id := Next_Entity (Id);

   begin
      if Present (Next) then
         Set_Prev_Entity (Next, Empty);  --  Empty <-- Next
      end if;

      Set_Next_Entity (Id, Empty);       --  Id --> Empty
   end Unlink_Next_Entity;

   ----------------------------------
   -- Is_Volatile, Set_Is_Volatile --
   ----------------------------------

   function Is_Volatile (Id : E) return B is
   begin
      --  ????The old version has a comment that says:
      --       The flag is not set reliably on private subtypes,
      --       and is always retrieved from the base type (but this is not a
      --       base-type-only attribute because it applies to other entities).
      --  Perhaps it should be set reliably, and perhaps it should be
      --  Base_Type_Only, but that doesn't work because it is currently
      --  set on subtypes, so we have to explicitly fetch the Base_Type below.
      --
      --  It might be cleaner if the call sites called Is_Volatile_Type
      --  or Is_Volatile_Object directly; surely they know which it is.

      pragma Assert (Nkind (Id) in N_Entity);

      if Is_Type (Id) then
         return Is_Volatile_Type (Base_Type (Id));
      else
         return Is_Volatile_Object (Id);
      end if;
   end Is_Volatile;

   procedure Set_Is_Volatile (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);

      if Is_Type (Id) then
         Set_Is_Volatile_Type (Id, V);
      else
         Set_Is_Volatile_Object (Id, V);
      end if;
   end Set_Is_Volatile;

   -----------------------
   -- Write_Entity_Info --
   -----------------------

   procedure Write_Entity_Info (Id : Entity_Id; Prefix : String) is

      procedure Write_Attribute (Which : String; Nam : E);
      --  Write attribute value with given string name

      procedure Write_Kind (Id : Entity_Id);
      --  Write Ekind field of entity

      ---------------------
      -- Write_Attribute --
      ---------------------

      procedure Write_Attribute (Which : String; Nam : E) is
      begin
         Write_Str (Prefix);
         Write_Str (Which);
         Write_Int (Int (Nam));
         Write_Str (" ");
         Write_Name (Chars (Nam));
         Write_Str (" ");
      end Write_Attribute;

      ----------------
      -- Write_Kind --
      ----------------

      procedure Write_Kind (Id : Entity_Id) is
         K : constant String := Entity_Kind'Image (Ekind (Id));

      begin
         Write_Str (Prefix);
         Write_Str ("   Kind    ");

         if Is_Type (Id) and then Is_Tagged_Type (Id) then
            Write_Str ("TAGGED ");
         end if;

         Write_Str (K (3 .. K'Length));
         Write_Str (" ");

         if Is_Type (Id) and then Depends_On_Private (Id) then
            Write_Str ("Depends_On_Private ");
         end if;
      end Write_Kind;

   --  Start of processing for Write_Entity_Info

   begin
      Write_Eol;
      Write_Attribute ("Name ", Id);
      Write_Int (Int (Id));
      Write_Eol;
      Write_Kind (Id);
      Write_Eol;
      Write_Attribute ("   Type    ", Etype (Id));
      Write_Eol;
      if Id /= Standard_Standard then
         Write_Attribute ("   Scope   ", Scope (Id));
      end if;
      Write_Eol;

      case Ekind (Id) is
         when Discrete_Kind =>
            Write_Str ("Bounds: Id = ");

            if Present (Scalar_Range (Id)) then
               Write_Int (Int (Type_Low_Bound (Id)));
               Write_Str (" .. Id = ");
               Write_Int (Int (Type_High_Bound (Id)));
            else
               Write_Str ("Empty");
            end if;

            Write_Eol;

         when Array_Kind =>
            declare
               Index : Entity_Id;

            begin
               Write_Attribute
                 ("   Component Type    ", Component_Type (Id));
               Write_Eol;
               Write_Str (Prefix);
               Write_Str ("   Indexes ");

               Index := First_Index (Id);
               while Present (Index) loop
                  Write_Attribute (" ", Etype (Index));
                  Index := Next_Index (Index);
               end loop;

               Write_Eol;
            end;

         when Access_Kind =>
               Write_Attribute
                 ("   Directly Designated Type ",
                  Directly_Designated_Type (Id));
               Write_Eol;

         when Overloadable_Kind =>
            if Present (Homonym (Id)) then
               Write_Str ("   Homonym   ");
               Write_Name (Chars (Homonym (Id)));
               Write_Str ("   ");
               Write_Int (Int (Homonym (Id)));
               Write_Eol;
            end if;

            Write_Eol;

         when E_Component =>
            if Ekind (Scope (Id)) in Record_Kind then
               Write_Attribute (
                  "   Original_Record_Component   ",
                  Original_Record_Component (Id));
               Write_Int (Int (Original_Record_Component (Id)));
               Write_Eol;
            end if;

         when others =>
            null;
      end case;
   end Write_Entity_Info;

   -------------------------
   -- Iterator Procedures --
   -------------------------

   procedure Proc_Next_Component                 (N : in out Node_Id) is
   begin
      N := Next_Component (N);
   end Proc_Next_Component;

   procedure Proc_Next_Component_Or_Discriminant (N : in out Node_Id) is
   begin
      N := Next_Entity (N);
      while Present (N) loop
         exit when Ekind (N) in E_Component | E_Discriminant;
         N := Next_Entity (N);
      end loop;
   end Proc_Next_Component_Or_Discriminant;

   procedure Proc_Next_Discriminant              (N : in out Node_Id) is
   begin
      N := Next_Discriminant (N);
   end Proc_Next_Discriminant;

   procedure Proc_Next_Formal                    (N : in out Node_Id) is
   begin
      N := Next_Formal (N);
   end Proc_Next_Formal;

   procedure Proc_Next_Formal_With_Extras        (N : in out Node_Id) is
   begin
      N := Next_Formal_With_Extras (N);
   end Proc_Next_Formal_With_Extras;

   procedure Proc_Next_Index                     (N : in out Node_Id) is
   begin
      N := Next_Index (N);
   end Proc_Next_Index;

   procedure Proc_Next_Inlined_Subprogram        (N : in out Node_Id) is
   begin
      N := Next_Inlined_Subprogram (N);
   end Proc_Next_Inlined_Subprogram;

   procedure Proc_Next_Literal                   (N : in out Node_Id) is
   begin
      N := Next_Literal (N);
   end Proc_Next_Literal;

   procedure Proc_Next_Stored_Discriminant       (N : in out Node_Id) is
   begin
      N := Next_Stored_Discriminant (N);
   end Proc_Next_Stored_Discriminant;

end Einfo.Utils;
