------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ C N V                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1992-2003 Free Software Foundation, Inc.         --
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

--  This package contains generic subprograms used for converting between
--  sequences of Character and Wide_Character. All access to wide character
--  sequences is isolated in this unit.

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

with System.WCh_Con;

package System.WCh_Cnv is
pragma Pure (WCh_Cnv);

   generic
      with function In_Char return Character;
   function Char_Sequence_To_Wide_Char
     (C    : Character;
      EM   : System.WCh_Con.WC_Encoding_Method)
      return Wide_Character;
   --  C is the first character of a sequence of one or more characters which
   --  represent a wide character sequence. Calling the function In_Char for
   --  additional characters as required, Char_To_Wide_Char returns the
   --  corresponding wide character value. Constraint_Error is raised if the
   --  sequence of characters encountered is not a valid wide character
   --  sequence for the given encoding method.

   generic
      with procedure Out_Char (C : Character);
   procedure Wide_Char_To_Char_Sequence
     (WC : Wide_Character;
      EM : System.WCh_Con.WC_Encoding_Method);
   --  Given a wide character, converts it into a sequence of one or
   --  more characters, calling the given Out_Char procedure for each.
   --  Constraint_Error is raised if the given wide character value is
   --  not a valid value for the given encoding method.

end System.WCh_Cnv;
