------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ S T W                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This package contains the routine used to convert strings to wide (wide)
--  strings for use by wide (wide) character attributes (value, image etc.)

with System.WCh_Con;

package System.WCh_StW is
pragma Pure (WCh_StW);

   function String_To_Wide_String
     (S  : String;
      EM : System.WCh_Con.WC_Encoding_Method) return Wide_String;
   --  This routine simply takes its argument and converts it to wide string
   --  format. In the context of the Wide_Image attribute, the argument is
   --  the corresponding 'Image attribute. Any wide character escape sequences
   --  in the string are converted to the corresponding wide character value.
   --  No syntax checks are made, it is assumed that any such sequences are
   --  validly formed (this must be assured by the caller), and results from
   --  the fact that Wide_Image is only used on strings that have been built
   --  by the compiler, such as images of enumeration literals. If the method
   --  for encoding is a shift-in, shift-out convention, then it is assumed
   --  that normal (non-wide character) mode holds at the start and end of
   --  the argument string. EM indicates the wide character encoding method.
   --  Note: in the WCEM_Brackets case, the brackets escape sequence is used
   --  only for codes greater than 16#FF#.

   function String_To_Wide_Wide_String
     (S  : String;
      EM : System.WCh_Con.WC_Encoding_Method) return Wide_Wide_String;
   --  Same function with Wide_Wide_String output

end System.WCh_StW;
