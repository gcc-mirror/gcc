------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 2                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2002 Free Software Foundation, Inc.          --
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

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Exp_Util; use Exp_Util;
with Nmake;    use Nmake;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Tbuild;   use Tbuild;

package body Exp_Ch12 is

   ------------------------------------
   -- Expand_N_Generic_Instantiation --
   ------------------------------------

   --  If elaboration entity is defined and this is not an outer level entity,
   --  we need to generate a check for it here.

   procedure Expand_N_Generic_Instantiation (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Entity_Id  := Entity (Name (N));

   begin
      if Etype (Name (N)) = Any_Type then
         return;
      end if;

      if Present (Elaboration_Entity (Ent))
        and then not Is_Compilation_Unit (Ent)
        and then not Elaboration_Checks_Suppressed (Ent)
      then
         Insert_Action (Instance_Spec (N),
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Not (Loc,
                 Right_Opnd =>
                   New_Occurrence_Of (Elaboration_Entity (Ent), Loc)),
             Reason => PE_Access_Before_Elaboration));
      end if;
   end Expand_N_Generic_Instantiation;

end Exp_Ch12;
