-- C455001.A

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687 and
--     F08630-91-C-0015, the U.S. Government obtained unlimited rights in the
--     software and documentation contained herein.  Unlimited rights are
--     defined in DFAR 252.227-7013(a)(19).  By making this public release,
--     the Government intends to confer upon all recipients unlimited rights
--     equal to those held by the Government.  These rights include rights to
--     use, duplicate, release or disclose the released technical data and
--     computer software in whole or in part, in any manner and for any purpose
--     whatsoever, and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--     Check that universal fixed multiplying operators can be used without
--     a conversion in contexts where the result type is determined.
--
--     Note: This is intended to check the changes made to these operators
--     in Ada 95; legacy tests should cover cases from Ada 83.
--
-- CHANGE HISTORY:
--      18 MAR 99   RLB    Initial version
--
--!

with Report; use Report;

procedure C455001 is

     type F1 is delta 2.0**(-1) range 0.0 .. 8.0;

     type F2 is delta 2.0**(-2) range 0.0 .. 4.0;

     type F3 is delta 2.0**(-3) range 0.0 .. 2.0;

     A : F1;
     B : F2;
     C : F3;

     type Fixed_Record is record
	D : F1;
        E : F2;
     end record;

     R : Fixed_Record;

     function Ident_Fix (X : F3) return F3 is
     begin
          if Equal(3,3) then
               return X;
          else
               return 0.0;
          end if;
     end Ident_Fix;

begin
     Test ("C455001", "Check that universal fixed multiplying operators " &
                      "can be used without a conversion in contexts where " &
                      "the result type is determined.");

     A := 1.0; B := 1.0;
     C := A * B; -- Assignment context.

     if C /= Ident_Fix(1.0) then
          Failed ("Incorrect results for multiplication (1) - result is " &
		  F3'Image(C));
     end if;

     C := A / B;

     if C /= Ident_Fix(1.0) then
          Failed ("Incorrect results for division (1) - result is " &
		  F3'Image(C));
     end if;

     A := 2.5;
     C := A * 0.25;

     if C /= Ident_Fix(0.625) then
          Failed ("Incorrect results for multiplication (2) - result is " &
		  F3'Image(C));
     end if;

     C := A / 4.0;

     if C /= Ident_Fix(0.625) then
          Failed ("Incorrect results for division (2) - result is " &
		  F3'Image(C));
     end if;

     C := Ident_Fix(0.75);
     C := C * 0.5;

     if C /= Ident_Fix(0.375) then
          Failed ("Incorrect results for multiplication (3) - result is " &
		  F3'Image(C));
     end if;

     C := Ident_Fix(0.75);
     C := C / 0.5;

     if C /= Ident_Fix(1.5) then
          Failed ("Incorrect results for division (3) - result is " &
		  F3'Image(C));
     end if;

     A := 0.5; B := 0.3; -- Function parameter context.
     if Ident_Fix(A * B) not in Ident_Fix(0.125) .. Ident_Fix(0.25) then
          Failed ("Incorrect results for multiplication (4) - result is " &
		  F3'Image(A * B)); -- Exact = 0.15
     end if;

     B := 0.8;
     if Ident_Fix(A / B) not in Ident_Fix(0.5) .. Ident_Fix(0.75) then
          Failed ("Incorrect results for division (4) - result is " &
		  F3'Image(A / B));
		-- Exact = 0.625..., but B is only restricted to the range
		-- 0.75 .. 1.0, so the result can be anywhere in the range
		-- 0.5 .. 0.75.
     end if;

     C := 0.875; B := 1.5;
     R := (D => C * 4.0, E => B / 0.5); -- Aggregate context.

     if R.D /= 3.5 then
          Failed ("Incorrect results for multiplication (5) - result is " &
		  F1'Image(R.D));
     end if;

     if R.E /= 3.0 then
          Failed ("Incorrect results for division (5) - result is " &
		  F2'Image(R.E));
     end if;

     A := 0.5;
     C := A * F1'(B * 2.0); -- Qualified expression context.

     if C /= Ident_Fix(1.5) then
          Failed ("Incorrect results for multiplication (6) - result is " &
		  F3'Image(C));
     end if;

     A := 4.0;
     C := F1'(B / 0.5) / A;

     if C /= Ident_Fix(0.75) then
          Failed ("Incorrect results for division (6) - result is " &
		  F3'Image(C));
     end if;

     Result;

end C455001;
