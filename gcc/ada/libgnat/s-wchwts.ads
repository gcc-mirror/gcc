------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ W T S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routine used to convert wide strings and wide
--  wide strings to strings for use by wide and wide wide character attributes
--  (value, image etc.) and also by the numeric IO subpackages of
--  Ada.Text_IO.Wide_Text_IO and Ada.Text_IO.Wide_Wide_Text_IO.

with System.WCh_Con;

package System.WCh_WtS is
   pragma Pure;

   function Wide_String_To_String
     (S  : Wide_String;
      EM : System.WCh_Con.WC_Encoding_Method) return String;
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
   --  for characters greater than 16#FF#. The lowest index of the returned
   --  String is equal to S'First.

   function Wide_Wide_String_To_String
     (S  : Wide_Wide_String;
      EM : System.WCh_Con.WC_Encoding_Method) return String;
   --  Same processing, except for Wide_Wide_String

end System.WCh_WtS;
