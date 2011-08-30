------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         E X P _ C H 6 _ L I G H T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2011, Free Software Foundation, Inc.          --
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
with Exp_Ch6;  use Exp_Ch6;
with Exp_Dbug; use Exp_Dbug;
with Rtsfind;  use Rtsfind;
with Sem_Aux;  use Sem_Aux;
with Sem_Res;  use Sem_Res;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Tbuild;   use Tbuild;

package body Exp_Ch6_Light is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_Simple_Function_Return (N : Node_Id);
   --  Expand simple return from function

   -----------------------
   -- Expand_Light_Call --
   -----------------------

   procedure Expand_Light_Call (N : Node_Id) is
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

   end Expand_Light_Call;

   --------------------------------------------
   -- Expand_Light_N_Simple_Return_Statement --
   --------------------------------------------

   procedure Expand_Light_N_Simple_Return_Statement (N : Node_Id) is
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
            Expand_Simple_Function_Return (N);

         when E_Procedure         |
              E_Generic_Procedure |
              E_Entry             |
              E_Entry_Family      |
              E_Return_Statement =>
            --  Expand_Non_Function_Return (N);
            null;

         when others =>
            raise Program_Error;
      end case;

   exception
      when RE_Not_Available =>
         return;
   end Expand_Light_N_Simple_Return_Statement;

   ------------------------------------
   -- Expand_Light_N_Subprogram_Body --
   ------------------------------------

   procedure Expand_Light_N_Subprogram_Body (N : Node_Id) is
   begin
      Qualify_Entity_Names (N);
   end Expand_Light_N_Subprogram_Body;

   -----------------------------------
   -- Expand_Simple_Function_Return --
   -----------------------------------

   procedure Expand_Simple_Function_Return (N : Node_Id) is
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
   end Expand_Simple_Function_Return;

end Exp_Ch6_Light;
