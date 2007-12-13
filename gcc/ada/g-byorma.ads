------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 G N A T . B Y T E _ O R D E R _ M A R K                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2007, AdaCore                     --
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

--  This package provides a procedure for reading and interpreting the BOM
--  (byte order mark) used to publish the encoding method for a string (for
--  example, a UTF-8 encoded file in windows will start with the appropriate
--  BOM sequence to signal UTF-8 encoding.

--  There are two cases

--    Case 1. UTF encodings for Unicode files

--      Here the convention is to have the first character of the file be a
--      non-breaking zero width space character (16#0000_FEFF#). For the UTF
--      encodings, the representation of this character can be used to uniquely
--      determine the encoding. Furthermore, the possibility of any confusion
--      with unencoded files is minimal, since for example the UTF-8 encoding
--      of this character looks like the sequence:

--        LC_I_Diaeresis
--        Right_Angle_Quotation
--        Fraction_One_Half

--      which is so unlikely to occur legitimately in normal use that it can
--      safely be ignored in most cases (for example, no legitimate Ada source
--      file could start with this sequence of characters).

--   Case 2. Specialized XML encodings

--     The XML standard defines a number of other possible encodings and also
--     defines standardized sequences for marking these encodings. This package
--     can also optionally handle these XML defined BOM sequences. These XML
--     cases depend on the first character of the XML file being < so that the
--     encoding of this character can be recognized.

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

package GNAT.Byte_Order_Mark is

   type BOM_Kind is
     (UTF8_All,  --  UTF8-encoding
      UTF16_LE,  --  UTF16 little-endian encoding
      UTF16_BE,  --  UTF16 big-endian encoding
      UTF32_LE,  --  UTF32 little-endian encoding
      UTF32_BE,  --  UTF32 big-endian encoding

      --  The following cases are for XML only

      UCS4_BE,   --  UCS-4, big endian machine (1234 order)
      UCS4_LE,   --  UCS-4, little endian machine (4321 order)
      UCS4_2143, --  UCS-4, unusual byte order (2143 order)
      UCS4_3412, --  UCS-4, unusual byte order (3412 order)

      --  Value returned if no BOM recognized

      Unknown);  --  Unknown, assumed to be ASCII compatible

   procedure Read_BOM
     (Str         : String;
      Len         : out Natural;
      BOM         : out BOM_Kind;
      XML_Support : Boolean := False);
   --  This is the routine to read the BOM from the start of the given string
   --  Str. On return BOM is set to the appropriate BOM_Kind and Len is set to
   --  its length. The caller will typically skip the first Len characters in
   --  the string to ignore the BOM sequence. The special XML possibilities are
   --  recognized only if flag XML_Support is set to True. Note that for the
   --  XML cases, Len is always set to zero on return (not to the length of the
   --  relevant sequence) since in the XML cases, the sequence recognized is
   --  for the first real character in the file (<) which is not to be skipped.

end GNAT.Byte_Order_Mark;
