------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                S Y S T E M . L I S T _ F I N A L I Z A T I O N           --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Finalization_Implementation;
package body Ada.Finalization.List_Controller is

   package SFI renames System.Finalization_Implementation;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out List_Controller) is
      use type SFR.Finalizable_Ptr;

      Last_Ptr : constant SFR.Finalizable_Ptr := Object.Last'Unchecked_Access;

   begin
      while Object.First.Next /= Last_Ptr loop
         SFI.Finalize_One (Object.First.Next.all);
      end loop;
   end Finalize;

   procedure Finalize (Object : in out Simple_List_Controller) is
   begin
      SFI.Finalize_List (Object.F);
      Object.F := null;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out List_Controller) is
   begin
      Object.F          := Object.First'Unchecked_Access;
      Object.First.Next := Object.Last 'Unchecked_Access;
      Object.Last.Prev  := Object.First'Unchecked_Access;
   end Initialize;

end Ada.Finalization.List_Controller;
