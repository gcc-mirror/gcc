------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        L O C A L _ R E S T R I C T                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Lib;            use Lib;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Util;       use Sem_Util;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;

package body Local_Restrict is
   function L_R_Image (L_R : Local_Restriction) return String is
     (case L_R is when No_Secondary_Stack => "No_Secondary_Stack",
        when No_Heap_Allocations => "No_Heap_Allocations");
   --  Like Local_Restriction'Image, but with casing appropriate for
   --  error messages.

   function Active_Restriction
     (L_R : Local_Restriction; E : Entity_Id := Current_Scope) return Node_Id;
   --  Returns the Local_Restrictions aspect specification that is in effect
   --  for E and that imposes the given local restriction; returns Empty if
   --  no such aspect specification exists.

   procedure Check_For_Corresponding_Local_Restriction
     (Violated : All_Restrictions; N : Node_Id);
   --  Generate error for node N if a violation of the given (non-local)
   --  restriction implies a violation of a corresponding local restriction
   --  that is in effect.

   procedure Check_For_Local_Restriction
     (L_R : Local_Restriction; N : Node_Id);
   --  Generate error for node N if given restriction is in effect.

   ------------------------
   -- Active_Restriction --
   ------------------------

   function Active_Restriction
     (L_R : Local_Restriction; E : Entity_Id := Current_Scope) return Node_Id
   is
      Scop   : Node_Id := E;
      Result : Node_Id;
   begin
      --  If performance of this function becomes a problem then
      --  one possible solution would be cache a set of scopes
      --  for which it is known that no local restrictions apply.
      --  Perhaps with fixed size (maybe 8?) and LRU replacement?
      --
      --  Or perhaps just a single global Boolean indicating that
      --  no Local_Restrictions aspect specification has been seen
      --  at any time in any context during the current compilation.
      --  If the flag is set, then Active_Restriction returns Empty
      --  without any looping.

      while Present (Scop) loop
         Result := Find_Aspect (Scop, Aspect_Local_Restrictions);
         if Present (Result)
           and then Parse_Aspect_Local_Restrictions (Result) (L_R)
         then
            return Result;
         end if;

         declare
            Saved_Scope : constant Node_Id := Scop;
         begin
            Scop := Enclosing_Declaration (Scop);
            if Present (Scop) then
               Scop := Parent (Scop);
               if Present (Scop) then
                  --  For a subprogram associated with a type, we don't care
                  --  where the type was frozen; continue from the type.

                  if Nkind (Scop) = N_Freeze_Entity then
                     Scop := Scope (Entity (Scop));
                  elsif Nkind (Parent (Scop)) = N_Freeze_Entity then
                     Scop := Scope (Entity (Parent (Scop)));
                  elsif Present (Scope (Saved_Scope)) then
                     Scop := Scope (Saved_Scope);
                  else
                     Scop := Find_Enclosing_Scope (Scop);
                  end if;
               end if;
            end if;
         end;
      end loop;

      return Empty;
   end Active_Restriction;

   -----------------------------------------------
   -- Check_For_Corresponding_Local_Restriction --
   -----------------------------------------------

   procedure Check_For_Corresponding_Local_Restriction
     (Violated : All_Restrictions; N : Node_Id)
   is
      L_R : Local_Restriction;
   begin
      --  Some restrictions map to a corresponding local restriction.
      --  In those cases, check whether the local restriction is in effect.
      --  This is the point at which the specific semantics of each
      --  local restriction is effectively defined.
      case Violated is
         when No_Secondary_Stack =>
            L_R := No_Secondary_Stack;
         when No_Allocators | No_Implicit_Heap_Allocations =>
            L_R := No_Heap_Allocations;
         when others =>
            return;
      end case;

      Check_For_Local_Restriction (L_R, N);
   end Check_For_Corresponding_Local_Restriction;

   ---------------------------------
   -- Check_For_Local_Restriction --
   ---------------------------------

   procedure Check_For_Local_Restriction
     (L_R : Local_Restriction; N : Node_Id)
   is
      L_R_Aspect_Spec : constant Node_Id := Active_Restriction (L_R);
   begin
      if Present (L_R_Aspect_Spec) then
         Error_Msg_Sloc := Sloc (L_R_Aspect_Spec);
         Error_Msg_N
           ("violation of local restriction " & L_R_Image (L_R) & "#", N);
      end if;
   end Check_For_Local_Restriction;

   ----------------
   -- Check_Call --
   ----------------

   procedure Check_Call (Call : Node_Id; Callee : Entity_Id := Empty) is
      Restrictions_Enforced_By_Callee : Local_Restriction_Set :=
        (others => False);

      Real_Callee : Entity_Id;
   begin
      if Present (Callee) then
         Real_Callee := Ultimate_Alias (Callee);

         if Is_Intrinsic_Subprogram (Real_Callee)
           or else In_Predefined_Unit (Real_Callee)
         then
            --  If an intrinsic or predefined subprogram violates a local
            --  restriction then we don't catch it here. For that, we rely
            --  on the same mechanism that is used to catch violations of
            --  the corresponding global restriction (i.e., the
            --  Local_Restriction_Checking_Hook call in Check_Restriction).
            return;
         end if;

         for L_R in Local_Restriction loop
            if Present (Active_Restriction (L_R, Real_Callee)) then
               Restrictions_Enforced_By_Callee (L_R) := True;
            end if;
         end loop;
      end if;

      for L_R in Local_Restriction loop
         if not Restrictions_Enforced_By_Callee (L_R) then
            --  Complain if caller must enforce L_R and callee
            --  does not promise to do that.

            Check_For_Local_Restriction (L_R, Call);
         end if;
      end loop;
   end Check_Call;

   -----------------------
   --  Check_Overriding --
   -----------------------

   procedure Check_Overriding (Overrider_Op, Overridden_Op : Entity_Id) is
      Ultimate_Overrider  : constant Entity_Id :=
        Ultimate_Alias (Overrider_Op);
      Ultimate_Overridden : constant Entity_Id :=
        Ultimate_Alias (Overridden_Op);
   begin
      --  a minor optimization
      if Ultimate_Overrider = Ultimate_Overridden then
         return;
      end if;

      for L_R in Local_Restriction loop
         if Present (Active_Restriction (L_R, Ultimate_Overridden))
           and then No (Active_Restriction (L_R, Ultimate_Overrider))
         then
            Error_Msg_Sloc :=
              Sloc (Active_Restriction (L_R, Ultimate_Overridden));
            Error_Msg_N
              ("overriding incompatible with local restriction " &
               L_R_Image (L_R) & "#",
               Ultimate_Overrider);
         end if;
      end loop;
   end Check_Overriding;

   ------------------------------------------
   -- Check_Actual_Subprogram_For_Instance --
   ------------------------------------------

   procedure Check_Actual_Subprogram_For_Instance
     (Actual_Subp_Name : Node_Id; Formal_Subp : Entity_Id)
   is
      Actual_Subp : Entity_Id := Empty;
   begin
      if Is_Entity_Name (Actual_Subp_Name) then
         Actual_Subp := Entity (Actual_Subp_Name);
      end if;

      for L_R in Local_Restriction loop
         --  Complain if some local restriction is in effect for
         --  the formal subprogram but not for the actual subprogram.

         if Present (Active_Restriction (L_R, Formal_Subp))
           and then
             (No (Actual_Subp)
               or else No (Active_Restriction (L_R, Actual_Subp)))
         then
            Error_Msg_Sloc := Sloc (Active_Restriction (L_R, Formal_Subp));
            Error_Msg_N
              ("actual subprogram incompatible with local restriction " &
               L_R_Image (L_R) & " #",
               Actual_Subp_Name);
         end if;
      end loop;
   end Check_Actual_Subprogram_For_Instance;

begin
   --  Allow package Restrict to call package Local_Restrict without
   --  pulling the bulk of semantics into the closure of package Restrict.
   --
   --  For example, if an allocator is encountered, then package
   --  Restrict is called to check whether a No_Allocators restriction is
   --  in effect. At that point, we also want to check whether a
   --  No_Heap_Allocations local restriction is in effect. This
   --  registration makes that possible.

   Local_Restrictions.Local_Restriction_Checking_Hook :=
     Check_For_Corresponding_Local_Restriction'Access;
end Local_Restrict;
