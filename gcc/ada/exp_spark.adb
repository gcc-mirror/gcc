------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            E X P _ S P A R K                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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
with Exp_Ch5;  use Exp_Ch5;
with Exp_Dbug; use Exp_Dbug;
with Exp_Util; use Exp_Util;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;

package body Exp_SPARK is

   -----------------------
   -- Local Subprograms --
   -----------------------

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

         when N_Expanded_Name |
              N_Identifier    =>
            Expand_Potential_Renaming (N);

         when N_Object_Renaming_Declaration =>
            Expand_SPARK_N_Object_Renaming_Declaration (N);

         --  Loop iterations over arrays need to be expanded, to avoid getting
         --  two names referring to the same object in memory (the array and
         --  the iterator) in GNATprove, especially since both can be written
         --  (thus possibly leading to interferences due to aliasing). No such
         --  problem arises with quantified expressions over arrays, which are
         --  dealt with specially in GNATprove.

         when N_Loop_Statement =>
            declare
               Scheme : constant Node_Id := Iteration_Scheme (N);
            begin
               if Present (Scheme)
                 and then Present (Iterator_Specification (Scheme))
                 and then
                   Is_Iterator_Over_Array (Iterator_Specification (Scheme))
               then
                  Expand_Iterator_Loop_Over_Array (N);
               end if;
            end;

         --  In SPARK mode, no other constructs require expansion

         when others =>
            null;
      end case;
   end Expand_SPARK;

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
      Id     : constant Entity_Id  := Entity (N);
      Loc    : constant Source_Ptr := Sloc (N);
      Typ    : constant Entity_Id  := Etype (N);
      Ren_Id : Node_Id;

   begin
      --  Replace a reference to a renaming with the actual renamed object

      if Ekind (Id) in Object_Kind then
         Ren_Id := Renamed_Object (Id);

         if Present (Ren_Id) then

            --  The renamed object is an entity when instantiating generics
            --  or inlining bodies. In this case the renaming is part of the
            --  mapping "prologue" which links actuals to formals.

            if Nkind (Ren_Id) in N_Entity then
               Rewrite (N, New_Occurrence_Of (Ren_Id, Loc));

            --  Otherwise the renamed object denotes a name

            else
               Rewrite (N, New_Copy_Tree (Ren_Id));
               Reset_Analyzed_Flags (N);
            end if;

            Analyze_And_Resolve (N, Typ);
         end if;
      end if;
   end Expand_Potential_Renaming;

end Exp_SPARK;
