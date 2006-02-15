------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ U T I L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

--  This package provides some common utilities used by the s-valxxx files

package System.Val_Util is
   pragma Pure;

   procedure Normalize_String
     (S    : in out String;
      F, L : out Integer);
   --  This procedure scans the string S setting F to be the index of the first
   --  non-blank character of S and L to be the index of the last non-blank
   --  character of S. Any lower case characters present in S will be folded
   --  to their upper case equivalent except for character literals. If S
   --  consists of entirely blanks then Constraint_Error is raised.
   --
   --  Note: if S is the null string, F is set to S'First, L to S'Last

   procedure Scan_Sign
     (Str   : String;
      Ptr   : access Integer;
      Max   : Integer;
      Minus : out Boolean;
      Start : out Positive);
   --  The Str, Ptr, Max parameters are as for the scan routines (Str is the
   --  string to be scanned starting at Ptr.all, and Max is the index of the
   --  last character in the string). Scan_Sign first scans out any initial
   --  blanks, raising Constraint_Error if the field is all blank. It then
   --  checks for and skips an initial plus or minus, requiring a non-blank
   --  character to follow (Constraint_Error is raised if plus or minus
   --  appears at the end of the string or with a following blank). Minus is
   --  set True if a minus sign was skipped, and False otherwise. On exit
   --  Ptr.all points to the character after the sign, or to the first
   --  non-blank character if no sign is present. Start is set to the point
   --  to the first non-blank character (sign or digit after it).
   --
   --  Note: if Str is null, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case. Constraint_Error is
   --  also raised in this case.

   procedure Scan_Plus_Sign
     (Str   : String;
      Ptr   : access Integer;
      Max   : Integer;
      Start : out Positive);
   --  Same as Scan_Sign, but allows only plus, not minus.
   --  This is used for modular types.

   function Scan_Exponent
     (Str  : String;
      Ptr  : access Integer;
      Max  : Integer;
      Real : Boolean := False) return Integer;
   --  Called to scan a possible exponent. Str, Ptr, Max are as described above
   --  for Scan_Sign. If Ptr.all < Max and Str (Ptr.all) = 'E' or 'e', then an
   --  exponent is scanned out, with the exponent value returned in Exp, and
   --  Ptr.all updated to point past the exponent. If the exponent field is
   --  incorrectly formed or not present, then Ptr.all is unchanged, and the
   --  returned exponent value is zero. Real indicates whether a minus sign
   --  is permitted (True = permitted). Very large exponents are handled by
   --  returning a suitable large value. If the base is zero, then any value
   --  is allowed, and otherwise the large value will either cause underflow
   --  or overflow during the scaling process which is fine.

   procedure Scan_Trailing_Blanks (Str : String; P : Positive);
   --  Checks that the remainder of the field Str (P .. Str'Last) is all
   --  blanks. Raises Constraint_Error if a non-blank character is found.

   procedure Scan_Underscore
     (Str : String;
      P   : in out Natural;
      Ptr : access Integer;
      Max : Integer;
      Ext : Boolean);
   --  Called if an underscore is encountered while scanning digits. Str (P)
   --  contains the underscore. Ptr it the pointer to be returned to the
   --  ultimate caller of the scan routine, Max is the maximum subscript in
   --  Str, and Ext indicates if extended digits are allowed. In the case
   --  where the underscore is invalid, Constraint_Error is raised with Ptr
   --  set appropriately, otherwise control returns with P incremented past
   --  the underscore.

end System.Val_Util;
