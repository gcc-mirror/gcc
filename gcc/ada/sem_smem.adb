------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ S M E M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1998-2000, Free Software Foundation, Inc.         --
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

with Atree;  use Atree;
with Einfo;  use Einfo;
with Errout; use Errout;
with Namet;  use Namet;
with Sinfo;  use Sinfo;
with Snames; use Snames;

package body Sem_Smem is

   function Contains_Access_Type (T : Entity_Id) return Boolean;
   --  This function determines if type T is an access type, or contains
   --  a component (array, record, protected type cases) that contains
   --  an access type (recursively defined in the appropriate manner).

   ----------------------
   -- Check_Shared_Var --
   ----------------------

   procedure Check_Shared_Var
     (Id : Entity_Id;
      T  : Entity_Id;
      N  : Node_Id)
   is
   begin
      --  We cannot tolerate aliased variables, because they might be
      --  modified via an aliased pointer, and we could not detect that
      --  this was happening (to update the corresponding shared memory
      --  file), so we must disallow all use of Aliased

      if Aliased_Present (N) then
         Error_Msg_N
           ("aliased variables " &
            "not supported in Shared_Passive partitions",
            N);

      --  We can't support access types at all, since they are local
      --  pointers that cannot in any simple way be transmitted to other
      --  partitions.

      elsif Is_Access_Type (T) then
         Error_Msg_N
           ("access type variables " &
            "not supported in Shared_Passive partitions",
            Id);

      --  We cannot tolerate types that contain access types, same reasons

      elsif Contains_Access_Type (T) then
         Error_Msg_N
           ("types containing access components " &
            "not supported in Shared_Passive partitions",
            Id);

      --  Currently we do not support unconstrained record types, since we
      --  use 'Write to write out values. This could probably be special
      --  cased and handled in the future if necessary.

      elsif Is_Record_Type (T)
        and then not Is_Constrained (T)
      then
         Error_Msg_N
           ("unconstrained variant records " &
            "not supported in Shared_Passive partitions",
            Id);
      end if;
   end Check_Shared_Var;

   --------------------------
   -- Contains_Access_Type --
   --------------------------

   function Contains_Access_Type (T : Entity_Id) return Boolean is
      C : Entity_Id;

   begin
      if Is_Access_Type (T) then
         return True;

      elsif Is_Array_Type (T) then
         return Contains_Access_Type (Component_Type (T));

      elsif Is_Record_Type (T) then
         if Has_Discriminants (T) then
            C := First_Discriminant (T);
            while Present (C) loop
               if Comes_From_Source (C) then
                  return True;
               else
                  C := Next_Discriminant (C);
               end if;
            end loop;
         end if;

         C := First_Component (T);
         while Present (C) loop

            --  For components, ignore internal components other than _Parent

            if Comes_From_Source (T)
              and then
                (Chars (C) = Name_uParent
                  or else
                 not Is_Internal_Name (Chars (C)))
              and then Contains_Access_Type (Etype (C))
            then
               return True;
            else
               C := Next_Component (C);
            end if;
         end loop;

         return False;

      elsif Is_Protected_Type (T) then
         return Contains_Access_Type (Corresponding_Record_Type (T));

      else
         return False;
      end if;
   end Contains_Access_Type;

end Sem_Smem;
