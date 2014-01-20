------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            E X P _ S P A R K                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
with Exp_Dbug; use Exp_Dbug;
with Exp_Util; use Exp_Util;
with Sem_Aux;  use Sem_Aux;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;

package body Exp_SPARK is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_SPARK_Call (N : Node_Id);
   --  This procedure contains common processing for function and procedure
   --  calls: replacement of renaming by subprogram renamed

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id);
   --  Perform name evaluation for a renamed object

   procedure Expand_Potential_Renaming (N : Node_Id);
   --  N denotes a N_Identifier or N_Expanded_Name. If N references a renaming,
   --  replace N with the renamed object.

   ------------------
   -- Expand_SPARK --
   ------------------

   procedure Expand_SPARK (N : Node_Id) is
   begin
      case Nkind (N) is

         --  Qualification of entity names in formal verification mode
         --  is limited to the addition of a suffix for homonyms (see
         --  Exp_Dbug.Qualify_Entity_Name). We used to qualify entity names
         --  as full expansion does, but this was removed as this prevents the
         --  verification back-end from using a short name for debugging and
         --  user interaction. The verification back-end already takes care
         --  of qualifying names when needed.

         when N_Block_Statement     |
              N_Package_Body        |
              N_Package_Declaration |
              N_Subprogram_Body     =>
            Qualify_Entity_Names (N);

         when N_Subprogram_Call     =>
            Expand_SPARK_Call (N);

         when N_Expanded_Name |
              N_Identifier    =>
            Expand_Potential_Renaming (N);

         when N_Object_Renaming_Declaration =>
            Expand_SPARK_N_Object_Renaming_Declaration (N);

         --  In SPARK mode, no other constructs require expansion

         when others =>
            null;
      end case;
   end Expand_SPARK;

   -----------------------
   -- Expand_SPARK_Call --
   -----------------------

   procedure Expand_SPARK_Call (N : Node_Id) is
   begin
      --  If the subprogram is a renaming, replace it in the call with the name
      --  of the actual subprogram being called. We distinguish renamings from
      --  inherited primitive operations, which both have an Alias component,
      --  by looking at the parent node of the entity. The entity for a
      --  renaming has the function or procedure specification node as
      --  parent, while an inherited primitive operation has the derived
      --  type declaration as parent.

      if Nkind (Name (N)) in N_Has_Entity
        and then Present (Entity (Name (N)))
      then
         declare
            E : constant Entity_Id := Entity (Name (N));
         begin
            if Nkind_In (Parent (E), N_Function_Specification,
                                     N_Procedure_Specification)
              and then Present (Alias (E))
            then
               Set_Entity (Name (N), Ultimate_Alias (E));
            end if;
         end;
      end if;
   end Expand_SPARK_Call;

   ------------------------------------------------
   -- Expand_SPARK_N_Object_Renaming_Declaration --
   ------------------------------------------------

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id) is
   begin
      --  Unconditionally remove all side effects from the name

      Evaluate_Name (Name (N));
   end Expand_SPARK_N_Object_Renaming_Declaration;

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

end Exp_SPARK;
