------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ U T I L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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
with System.Val_Spec;

package System.Val_Util
  with SPARK_Mode, Pure
is
   pragma Unevaluated_Use_Of_Old (Allow);

   package Sp renames System.Val_Spec;

   procedure Bad_Value (S : String)
   with
     Always_Terminates,
     Depends => (null => S),
     Exceptional_Cases => (others => Standard.False);
   pragma No_Return (Bad_Value);
   --  Raises constraint error with message: bad input for 'Value: "xxx"

   procedure Normalize_String
     (S             : in out String;
      F, L          : out Integer;
      To_Upper_Case : Boolean)
   with
     Post => (if Sp.Only_Space_Ghost (S'Old, S'First, S'Last) then
                F > L
              else
                F >= S'First
                  and then L <= S'Last
                  and then F <= L
                  and then Sp.Only_Space_Ghost (S'Old, S'First, F - 1)
                  and then S'Old (F) /= ' '
                  and then S'Old (L) /= ' '
                  and then
                    (if L < S'Last then
                      Sp.Only_Space_Ghost (S'Old, L + 1, S'Last))
                  and then
                    (if To_Upper_Case and then S'Old (F) /= ''' then
                      (for all J in S'Range =>
                        (if J in F .. L then
                           S (J) = System.Case_Util.To_Upper (S'Old (J))
                         else
                           S (J) = S'Old (J)))));
   --  This procedure scans the string S setting F to be the index of the first
   --  non-blank character of S and L to be the index of the last non-blank
   --  character of S. If To_Upper_Case is True and S does not represent a
   --  character literal, then any lower case characters in S are changed to
   --  their upper case counterparts. If S consists of only blank characters
   --  (including when S = "") then we return with F > L.

   procedure Scan_Sign
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Minus : out Boolean;
      Start : out Positive)
   with
     Pre  =>
       --  Ptr.all .. Max is either an empty range, or a valid range in Str
       (Ptr.all > Max or else (Ptr.all >= Str'First and then Max <= Str'Last))
       and then not Sp.Only_Space_Ghost (Str, Ptr.all, Max)
       and then
         (declare
            F : constant Positive :=
              Sp.First_Non_Space_Ghost (Str, Ptr.all, Max);
          begin
            (if Str (F) in '+' | '-' then
               F <= Max - 1 and then Str (F + 1) /= ' ')),
     Post =>
       (declare
          F : constant Positive :=
            Sp.First_Non_Space_Ghost (Str, Ptr.all'Old, Max);
        begin
          Minus = (Str (F) = '-')
            and then Ptr.all = (if Str (F) in '+' | '-' then F + 1 else F)
            and then Start = F);
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
      Start : out Positive)
   with
     Pre  =>
       --  Ptr.all .. Max is either an empty range, or a valid range in Str
       (Ptr.all > Max or else (Ptr.all >= Str'First and then Max <= Str'Last))
       and then not Sp.Only_Space_Ghost (Str, Ptr.all, Max)
       and then
         (declare
            F : constant Positive :=
              Sp.First_Non_Space_Ghost (Str, Ptr.all, Max);
          begin
            (if Str (F) = '+' then
               F <= Max - 1 and then Str (F + 1) /= ' ')),
     Post =>
       (declare
          F : constant Positive :=
            Sp.First_Non_Space_Ghost (Str, Ptr.all'Old, Max);
        begin
          Ptr.all = (if Str (F) = '+' then F + 1 else F)
            and then Start = F);
   --  Same as Scan_Sign, but allows only plus, not minus. This is used for
   --  modular types.

   procedure Scan_Exponent
     (Str  : String;
      Ptr  : not null access Integer;
      Max  : Integer;
      Exp  : out Integer;
      Real : Boolean := False)
   with
     Pre =>
       --  Ptr.all .. Max is either an empty range, or a valid range in Str
       (Ptr.all > Max or else (Ptr.all >= Str'First and then Max <= Str'Last))
         and then Max < Natural'Last
         and then Sp.Is_Opt_Exponent_Format_Ghost (Str (Ptr.all .. Max), Real),
     Post =>
       (if Sp.Starts_As_Exponent_Format_Ghost (Str (Ptr.all'Old .. Max), Real)
        then Exp = Sp.Scan_Exponent_Ghost (Str (Ptr.all'Old .. Max), Real)
          and then
          (if Str (Ptr.all'Old + 1) in '-' | '+' then
             Ptr.all = Sp.Last_Number_Ghost (Str (Ptr.all'Old + 2 .. Max)) + 1
           else
             Ptr.all = Sp.Last_Number_Ghost (Str (Ptr.all'Old + 1 .. Max)) + 1)
        else Exp = 0 and Ptr.all = Ptr.all'Old);
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

   procedure Scan_Trailing_Blanks (Str : String; P : Positive)
   with
     Pre => P >= Str'First
       and then Sp.Only_Space_Ghost (Str, P, Str'Last);
   --  Checks that the remainder of the field Str (P .. Str'Last) is all
   --  blanks. Raises Constraint_Error if a non-blank character is found.

   pragma Warnings
     (GNATprove, Off, """Ptr"" is not modified",
      Reason => "Ptr is actually modified when raising an exception");
   procedure Scan_Underscore
     (Str : String;
      P   : in out Natural;
      Ptr : not null access Integer;
      Max : Integer;
      Ext : Boolean)
   with
     Pre  => P in Str'Range
       and then Str (P) = '_'
       and then Max in Str'Range
       and then P < Max
       and then
         (if Ext then
            Str (P + 1) in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'
          else
            Str (P + 1) in '0' .. '9'),
     Post =>
       P = P'Old + 1
         and then Ptr.all'Old = Ptr.all;
   --  Called if an underscore is encountered while scanning digits. Str (P)
   --  contains the underscore. Ptr is the pointer to be returned to the
   --  ultimate caller of the scan routine, Max is the maximum subscript in
   --  Str, and Ext indicates if extended digits are allowed. In the case
   --  where the underscore is invalid, Constraint_Error is raised with Ptr
   --  set appropriately, otherwise control returns with P incremented past
   --  the underscore.
   --
   --  This routine must not be called with Str'Last = Positive'Last. There is
   --  no check for this case, the caller must ensure this condition is met.
   pragma Warnings (GNATprove, On, """Ptr"" is not modified");

end System.Val_Util;
