------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A L F A                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Exp_Attr; use Exp_Attr;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Dbug; use Exp_Dbug;
with Exp_Util; use Exp_Util;
with Nlists;   use Nlists;
with Rtsfind;  use Rtsfind;
with Sem_Aux;  use Sem_Aux;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;

package body Exp_Alfa is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Alfa_Call (N : Node_Id);
   --  This procedure contains common processing for function and procedure
   --  calls:
   --    * expansion of actuals to introduce necessary temporaries
   --    * replacement of renaming by subprogram renamed

   procedure Expand_Alfa_N_Attribute_Reference (N : Node_Id);
   --  Expand attributes 'Old and 'Result only

   procedure Expand_Alfa_N_In (N : Node_Id);
   --  Expand set membership into individual ones

   procedure Expand_Alfa_N_Object_Renaming_Declaration (N : Node_Id);
   --  Perform name evaluation for a renamed object

   procedure Expand_Alfa_N_Simple_Return_Statement (N : Node_Id);
   --  Insert conversion on function return if necessary

   procedure Expand_Alfa_Simple_Function_Return (N : Node_Id);
   --  Expand simple return from function

   procedure Expand_Potential_Renaming (N : Node_Id);
   --  N denotes a N_Identifier or N_Expanded_Name. If N references a renaming,
   --  replace N with the renamed object.

   -----------------
   -- Expand_Alfa --
   -----------------

   procedure Expand_Alfa (N : Node_Id) is
   begin
      case Nkind (N) is
         when N_Attribute_Reference =>
            Expand_Alfa_N_Attribute_Reference (N);

         when N_Block_Statement     |
              N_Package_Body        |
              N_Package_Declaration |
              N_Subprogram_Body     =>
            Qualify_Entity_Names (N);

         when N_Subprogram_Call     =>
            Expand_Alfa_Call (N);

         when N_Expanded_Name |
              N_Identifier    =>
            Expand_Potential_Renaming (N);

         when N_In =>
            Expand_Alfa_N_In (N);

         when N_Not_In =>
            Expand_N_Not_In (N);

         when N_Object_Renaming_Declaration =>
            Expand_Alfa_N_Object_Renaming_Declaration (N);

         when N_Simple_Return_Statement =>
            Expand_Alfa_N_Simple_Return_Statement (N);

         when others =>
            null;
      end case;
   end Expand_Alfa;

   ----------------------
   -- Expand_Alfa_Call --
   ----------------------

   procedure Expand_Alfa_Call (N : Node_Id) is
      Call_Node   : constant Node_Id := N;
      Parent_Subp : Entity_Id;
      Subp        : Entity_Id;

   begin
      --  Ignore if previous error

      if Nkind (Call_Node) in N_Has_Etype
        and then Etype (Call_Node) = Any_Type
      then
         return;
      end if;

      --  Call using access to subprogram with explicit dereference

      if Nkind (Name (Call_Node)) = N_Explicit_Dereference then
         Subp        := Etype (Name (Call_Node));
         Parent_Subp := Empty;

      --  Case of call to simple entry, where the Name is a selected component
      --  whose prefix is the task, and whose selector name is the entry name

      elsif Nkind (Name (Call_Node)) = N_Selected_Component then
         Subp        := Entity (Selector_Name (Name (Call_Node)));
         Parent_Subp := Empty;

      --  Case of call to member of entry family, where Name is an indexed
      --  component, with the prefix being a selected component giving the
      --  task and entry family name, and the index being the entry index.

      elsif Nkind (Name (Call_Node)) = N_Indexed_Component then
         Subp        := Entity (Selector_Name (Prefix (Name (Call_Node))));
         Parent_Subp := Empty;

      --  Normal case

      else
         Subp        := Entity (Name (Call_Node));
         Parent_Subp := Alias (Subp);
      end if;

      --  Various expansion activities for actuals are carried out

      Expand_Actuals (N, Subp);

      --  If the subprogram is a renaming, replace it in the call with the name
      --  of the actual subprogram being called.

      if Present (Parent_Subp) then
         Parent_Subp := Ultimate_Alias (Parent_Subp);

         --  The below setting of Entity is suspect, see F109-018 discussion???

         Set_Entity (Name (Call_Node), Parent_Subp);
      end if;
   end Expand_Alfa_Call;

   ---------------------------------------
   -- Expand_Alfa_N_Attribute_Reference --
   ---------------------------------------

   procedure Expand_Alfa_N_Attribute_Reference (N : Node_Id) is
      Id : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));

   begin
      case Id is
         when Attribute_Old    |
              Attribute_Result =>
            Expand_N_Attribute_Reference (N);

         when others =>
            null;
      end case;
   end Expand_Alfa_N_Attribute_Reference;

   ----------------------
   -- Expand_Alfa_N_In --
   ----------------------

   procedure Expand_Alfa_N_In (N : Node_Id) is
   begin
      if Present (Alternatives (N)) then
         Expand_Set_Membership (N);
      end if;
   end Expand_Alfa_N_In;

   -----------------------------------------------
   -- Expand_Alfa_N_Object_Renaming_Declaration --
   -----------------------------------------------

   procedure Expand_Alfa_N_Object_Renaming_Declaration (N : Node_Id) is
   begin
      --  Unconditionally remove all side effects from the name

      Evaluate_Name (Name (N));
   end Expand_Alfa_N_Object_Renaming_Declaration;

   -------------------------------------------
   -- Expand_Alfa_N_Simple_Return_Statement --
   -------------------------------------------

   procedure Expand_Alfa_N_Simple_Return_Statement (N : Node_Id) is
   begin
      --  Defend against previous errors (i.e. the return statement calls a
      --  function that is not available in configurable runtime).

      if Present (Expression (N))
        and then Nkind (Expression (N)) = N_Empty
      then
         return;
      end if;

      --  Distinguish the function and non-function cases:

      case Ekind (Return_Applies_To (Return_Statement_Entity (N))) is

         when E_Function          |
              E_Generic_Function  =>
            Expand_Alfa_Simple_Function_Return (N);

         when E_Procedure         |
              E_Generic_Procedure |
              E_Entry             |
              E_Entry_Family      |
              E_Return_Statement =>
            null;

         when others =>
            raise Program_Error;
      end case;

   exception
      when RE_Not_Available =>
         return;
   end Expand_Alfa_N_Simple_Return_Statement;

   ----------------------------------------
   -- Expand_Alfa_Simple_Function_Return --
   ----------------------------------------

   procedure Expand_Alfa_Simple_Function_Return (N : Node_Id) is
      Scope_Id : constant Entity_Id :=
                   Return_Applies_To (Return_Statement_Entity (N));
      --  The function we are returning from

      R_Type : constant Entity_Id := Etype (Scope_Id);
      --  The result type of the function

      Exp : constant Node_Id := Expression (N);
      pragma Assert (Present (Exp));

      Exptyp : constant Entity_Id := Etype (Exp);
      --  The type of the expression (not necessarily the same as R_Type)

   begin
      --  Check the result expression of a scalar function against the subtype
      --  of the function by inserting a conversion. This conversion must
      --  eventually be performed for other classes of types, but for now it's
      --  only done for scalars.
      --  ???

      if Is_Scalar_Type (Exptyp) then
         Rewrite (Exp, Convert_To (R_Type, Exp));

         --  The expression is resolved to ensure that the conversion gets
         --  expanded to generate a possible constraint check.

         Analyze_And_Resolve (Exp, R_Type);
      end if;
   end Expand_Alfa_Simple_Function_Return;

   -------------------------------
   -- Expand_Potential_Renaming --
   -------------------------------

   procedure Expand_Potential_Renaming (N : Node_Id) is
      E : constant Entity_Id := Entity (N);
      T : constant Entity_Id := Etype (N);

   begin
      --  Replace a reference to a renaming with the actual renamed object

      if Ekind (E) in Object_Kind and then Present (Renamed_Object (E)) then
         Rewrite (N, New_Copy_Tree (Renamed_Object (E)));
         Reset_Analyzed_Flags (N);
         Analyze_And_Resolve (N, T);
      end if;
   end Expand_Potential_Renaming;

end Exp_Alfa;
