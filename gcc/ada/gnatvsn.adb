------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T V S N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 2002 Free Software Foundation, Inc.               --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body Gnatvsn is

   --  Import the string constant defined in the (language-independent)
   --  source file version.c.

   --  The size is a lie; we have no way of writing the truth (the size
   --  is variable and depends on the actual text of the constant).

   --  FIXME: It should be possible to declare this to be a constant, but
   --  that is rejected by the compiler ("invalid context for deferred
   --  constant declaration").  Per Ada95 this constraint only applies to
   --  deferred constants completed by a full constant declaration, not
   --  deferred constants completed by a pragma Import.

   Version_String : array (0 .. Ver_Len_Max) of aliased Character;
   pragma Import (C, Version_String, "version_string");

   --  Convert that string constant to an Ada String and return it.
   --  This is essentially the same as the To_Ada routine in
   --  Interfaces.C; that package is not linked into gnat1 so
   --  we cannot use it.

   function Gnat_Version_String return String
   is
      Count : Natural := 0;

   begin
      loop
         if Version_String (Count) = Character'First then
            exit;
         else
            Count := Count + 1;
         end if;
      end loop;

      declare
         R : String (1 .. Count);

      begin
         for J in R'Range loop
            R (J) := Version_String (J - 1);
         end loop;

         return R;
      end;
   end Gnat_Version_String;

end Gnatvsn;
