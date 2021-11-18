------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ U T I L                       --
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

--  This package provides some common utilities used by the s-valxxx files

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with System.Case_Util;

package System.Val_Util
  with SPARK_Mode, Pure
is

   procedure Bad_Value (S : String)
   with
     Depends => (null => S);
   pragma No_Return (Bad_Value);
   --  Raises constraint error with message: bad input for 'Value: "xxx"

   function Only_Space_Ghost (S : String; From, To : Integer) return Boolean is
      (for all J in From .. To => S (J) = ' ')
   with
     Ghost,
     Pre => From > To or else (From >= S'First and then To <= S'Last);
   --  Ghost function that returns True if S has only space characters from
   --  index From to index To.

   procedure Normalize_String
     (S    : in out String;
      F, L : out Integer)
   with
     Post => (if Only_Space_Ghost (S'Old, S'First, S'Last) then
                F > L
              else
                F >= S'First
                  and then L <= S'Last
                  and then F <= L
                  and then Only_Space_Ghost (S'Old, S'First, F - 1)
                  and then S'Old (F) /= ' '
                  and then S'Old (L) /= ' '
                  and then
                    (if L < S'Last then
                      Only_Space_Ghost (S'Old, L + 1, S'Last))
                  and then
                    (if S'Old (F) /= ''' then
                      (for all J in S'Range =>
                        (if J in F .. L then
                           S (J) = System.Case_Util.To_Upper (S'Old (J))
                         else
                           S (J) = S'Old (J)))));
   --  This procedure scans the string S setting F to be the index of the first
   --  non-blank character of S and L to be the index of the last non-blank
   --  character of S. Any lower case characters present in S will be folded to
   --  their upper case equivalent except for character literals. If S consists
   --  of entirely blanks (including when S = "") then we return with F > L.

   procedure Scan_Sign
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Minus : out Boolean;
      Start : out Positive);
   --  The Str, Ptr, Max parameters are as for the scan routines (Str is the
   --  string to be scanned starting at Ptr.all, and Max is the index of the
   --  last character in the string). Scan_Sign first scans out any initial
   --  blanks, raising Constraint_Error if the field is all blank. It then
   --  checks for and skips an initial plus or minus, requiring a non-blank
   --  character to follow (Constraint_Error is raised if plus or minus appears
   --  at the end of the string or with a following blank). Minus is set True
   --  if a minus sign was skipped, and False otherwise. On exit Ptr.all points
   --  to the character after the sign, or to the first non-blank character
   --  if no sign is present. Start is set to the point to the first non-blank
   --  character (sign or digit after it).
   --
   --  Note: if Str is null, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case. Constraint_Error is also
   --  raised in this case.
   --
   --  This routine must not be called with Str'Last = Positive'Last. There is
   --  no check for this case, the caller must ensure this condition is met.

   procedure Scan_Plus_Sign
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Start : out Positive);
   --  Same as Scan_Sign, but allows only plus, not minus. This is used for
   --  modular types.

   function Scan_Exponent
     (Str  : String;
      Ptr  : not null access Integer;
      Max  : Integer;
      Real : Boolean := False) return Integer
   with
     SPARK_Mode => Off;  --  Function with side-effect through Ptr
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
   --
   --  This routine must not be called with Str'Last = Positive'Last. There is
   --  no check for this case, the caller must ensure this condition is met.

   procedure Scan_Trailing_Blanks (Str : String; P : Positive);
   --  Checks that the remainder of the field Str (P .. Str'Last) is all
   --  blanks. Raises Constraint_Error if a non-blank character is found.

   procedure Scan_Underscore
     (Str : String;
      P   : in out Natural;
      Ptr : not null access Integer;
      Max : Integer;
      Ext : Boolean);
   --  Called if an underscore is encountered while scanning digits. Str (P)
   --  contains the underscore. Ptr it the pointer to be returned to the
   --  ultimate caller of the scan routine, Max is the maximum subscript in
   --  Str, and Ext indicates if extended digits are allowed. In the case
   --  where the underscore is invalid, Constraint_Error is raised with Ptr
   --  set appropriately, otherwise control returns with P incremented past
   --  the underscore.
   --
   --  This routine must not be called with Str'Last = Positive'Last. There is
   --  no check for this case, the caller must ensure this condition is met.

end System.Val_Util;
