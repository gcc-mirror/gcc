------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ S T W                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package contains the routine used to convert strings to wide (wide)
--  strings for use by wide (wide) image attribute.

with System.WCh_Con;

package System.WCh_StW is
   pragma Pure;

   procedure String_To_Wide_String
     (S  : String;
      R  : out Wide_String;
      L  : out Natural;
      EM : System.WCh_Con.WC_Encoding_Method);
   --  This routine simply takes its argument and converts it to wide string
   --  format, storing the result in R (1 .. L), with L being set appropriately
   --  on return. The caller guarantees that R is long enough to accommodate
   --  the result. This is used in the context of the Wide_Image attribute,
   --  where the argument is the corresponding 'Image attribute. Any wide
   --  character escape sequences in the string are converted to the
   --  corresponding wide character value. No syntax checks are made, it is
   --  assumed that any such sequences are validly formed (this must be assured
   --  by the caller), and results from the fact that Wide_Image is only used
   --  on strings that have been built by the compiler, such as images of
   --  enumeration literals. If the method for encoding is a shift-in,
   --  shift-out convention, then it is assumed that normal (non-wide
   --  character) mode holds at the start and end of the argument string. EM
   --  indicates the wide character encoding method.
   --  Note: in the WCEM_Brackets case, the brackets escape sequence is used
   --  only for codes greater than 16#FF#.

   procedure String_To_Wide_Wide_String
     (S  : String;
      R  : out Wide_Wide_String;
      L  : out Natural;
      EM : System.WCh_Con.WC_Encoding_Method);
   --  Same function with Wide_Wide_String output

end System.WCh_StW;
