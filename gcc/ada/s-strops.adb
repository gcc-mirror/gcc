------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . S T R I N G _ O P S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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

package body System.String_Ops is

   ----------------
   -- Str_Concat --
   ----------------

   function Str_Concat (X, Y : String) return String is
   begin
      if X'Length <= 0 then
         return Y;

      else
         declare
            L : constant Natural := X'Length + Y'Length;
            R : String (X'First .. X'First + L - 1);

         begin
            R (X'Range) := X;
            R (X'First + X'Length .. R'Last) := Y;
            return R;
         end;
      end if;
   end Str_Concat;

   -------------------
   -- Str_Concat_CC --
   -------------------

   function Str_Concat_CC (X, Y : Character) return String is
      R : String (1 .. 2);

   begin
      R (1) := X;
      R (2) := Y;
      return R;
   end Str_Concat_CC;

   -------------------
   -- Str_Concat_CS --
   -------------------

   function Str_Concat_CS (X : Character; Y : String) return String is
      R : String (1 .. Y'Length + 1);

   begin
      R (1) := X;
      R (2 .. R'Last) := Y;
      return R;
   end Str_Concat_CS;

   -------------------
   -- Str_Concat_SC --
   -------------------

   function Str_Concat_SC (X : String; Y : Character) return String is
   begin
      if X'Length <= 0 then
         return (1 => Y);

      else
         declare
            R : String (X'First .. X'Last + 1);

         begin
            R (X'Range) := X;
            R (R'Last) := Y;
            return R;
         end;
      end if;
   end Str_Concat_SC;

   ---------------
   -- Str_Equal --
   ---------------

   function Str_Equal (A, B : String) return Boolean is
   begin
      if A'Length /= B'Length then
         return False;

      else
         for J in A'Range loop
            if A (J) /= B (J + (B'First - A'First)) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Str_Equal;

   -------------------
   -- Str_Normalize --
   -------------------

   procedure Str_Normalize (A : in out String) is
   begin
      for J in A'Range loop
         A (J) := Character'Last;
      end loop;
   end Str_Normalize;

   ------------------------
   -- Wide_Str_Normalize --
   ------------------------

   procedure Wide_Str_Normalize (A : in out Wide_String) is
   begin
      for J in A'Range loop
         A (J) := Wide_Character'Last;
      end loop;
   end Wide_Str_Normalize;

end System.String_Ops;
