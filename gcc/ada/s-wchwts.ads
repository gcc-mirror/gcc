------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . W C H _ W T S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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

--  This package contains the routine used to convert wide strings to
--  strings for use by wide character attributes (value, image etc.) and
--  also by the numeric IO subpackages of Ada.Text_IO.Wide_Text_IO.

with System.WCh_Con;

package System.WCh_WtS is
pragma Pure (WCh_WtS);

   function Wide_String_To_String
     (S    : Wide_String;
      EM   : System.WCh_Con.WC_Encoding_Method)
      return String;
   --  This routine simply takes its argument and converts it to a string,
   --  using the internal compiler escape sequence convention (defined in
   --  package Widechar) to translate characters that are out of range
   --  of type String. In the context of the Wide_Value attribute, the
   --  argument is the original attribute argument, and the result is used
   --  in a call to the corresponding Value attribute function. If the method
   --  for encoding is a shift-in, shift-out convention, then it is assumed
   --  that normal (non-wide character) mode holds at the start and end of
   --  the result string. EM indicates the wide character encoding method.
   --  Note: in the WCEM_Brackets case, we only use the brackets encoding
   --  for characters greater than 16#FF#.

end System.WCh_WtS;
