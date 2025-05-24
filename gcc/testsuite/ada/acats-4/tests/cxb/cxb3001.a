-- CXB3001.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that the specifications of the package Interfaces.C are
--      available for use.
--
-- TEST DESCRIPTION: 
--      This test verifies that the types and subprograms specified for the
--      interface are present.  It just checks for the presence of
--      the subprograms.  Other tests are designed to exercise the interface.
--
-- APPLICABILITY CRITERIA: 
--      If an implementation provides package Interfaces.C, this test
--      must compile, execute, and report "PASSED".
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 Nov 95   SAIC    Corrected To_C parameter list for ACVC 2.0.1.
--      28 Feb 96   SAIC    Added applicability criteria.
--
--!

with Report;
with Interfaces.C;                                             -- N/A => ERROR

procedure CXB3001 is
   package C renames Interfaces.C;
   use type C.signed_char;
   use type C.unsigned_char;
   use type C.char;

begin

   Report.Test ("CXB3001", "Check the specification of Interfaces.C");

   declare  -- encapsulate the test


      tst_CHAR_BIT  : constant :=  C.CHAR_BIT;
      tst_SCHAR_MIN : constant :=  C.SCHAR_MIN;
      tst_SCHAR_MAX : constant :=  C.SCHAR_MAX;
      tst_UCHAR_MAX : constant :=  C.UCHAR_MAX;

      -- Signed and Unsigned Integers

      tst_int   : C.int   := C.int'first;
      tst_short : C.short := C.short'first;
      tst_long  : C.long  := C.long'first;

      tst_signed_char_min : C.signed_char :=  C.signed_char'first;
      tst_signed_char_max : C.signed_char :=  C.signed_char'last;

      tst_unsigned        : C.unsigned;
      tst_unsigned_short  : C.unsigned_short;
      tst_unsigned_long   : C.unsigned_long;

      tst_unsigned_char :  C.unsigned_char;
      tst_plain_char    :  C.plain_char;

      tst_ptrdiff_t : C.ptrdiff_t;
      tst_size_t    : C.size_t;

      -- Floating-Point

      tst_C_float     : C.C_float;
      tst_double      : C.double;
      tst_long_double : C.long_double; 

      -- Characters and Strings

      tst_char : C.char;
      tst_nul  : C.char := C.nul;

      -- Collect all the subprogram calls such that they are compiled
      -- but not executed
      --
      procedure Collect_All_Calls is

         CAC_char           : C.char;
         CAC_Character      : Character;
         CAC_String         : string (1..5);
         CAC_Boolean        : Boolean := false;
         CAC_char_array     : C.char_array(1..5);
         CAC_Integer        : integer;
         CAC_Natural        : natural;
         CAC_wchar_t        : C.wchar_t;
         CAC_Wide_Character : Wide_Character;
         CAC_wchar_array    : C.wchar_array(1..5);
         CAC_Wide_String    : Wide_String(1..5);
         CAC_size_t         : C.size_t;

      begin

         CAC_char      := C.To_C (CAC_Character);
         CAC_Character := C.To_Ada (CAC_char);

         CAC_char_array :=  C.To_C (CAC_String, CAC_Boolean);
         CAC_String     :=  C.To_Ada (CAC_char_array, CAC_Boolean); 

         -- This call is out of LRM order so that we can use the 
         -- array initialized above
         CAC_Boolean  := C.Is_Nul_Terminated (CAC_char_array);

         C.To_C   (CAC_String, CAC_char_array, CAC_size_t, CAC_Boolean);
         C.To_Ada (CAC_char_array, CAC_String, CAC_Natural, CAC_Boolean); 

         CAC_wchar_t        := C.To_C (CAC_Wide_Character);
         CAC_Wide_Character := C.To_Ada (CAC_wchar_t); 
         CAC_wchar_t        := C.wide_nul; 

         CAC_wchar_array :=  C.To_C   (CAC_Wide_String, CAC_Boolean);
         CAC_Wide_String :=  C.To_Ada (CAC_wchar_array, CAC_Boolean);

         -- This call is out of LRM order so that we can use the 
         -- array initialized above
         CAC_Boolean  := C.Is_Nul_Terminated (CAC_wchar_array);

         C.To_C   (CAC_Wide_String, CAC_wchar_array, CAC_size_t,  CAC_Boolean);
         C.To_Ada (CAC_wchar_array, CAC_Wide_String, CAC_Natural, CAC_Boolean);

         raise C.Terminator_Error;

      end Collect_All_Calls;



   begin    -- encapsulation

      if  tst_signed_char_min /= C.SCHAR_MIN then
         Report.Failed  ("tst_signed_char_min is incorrect");
      end if;
      if  tst_signed_char_max /= C.SCHAR_MAX then
         Report.Failed  ("tst_signed_char_max is incorrect");
      end if;
      if C.signed_char'Size /= C.CHAR_BIT then
         Report.Failed  ("C.signed_char'Size is incorrect");
      end if;

      if C.unsigned_char'first /= 0                or
         C.unsigned_char'last  /= C.UCHAR_MAX      or
         C.unsigned_char'size  /= C.CHAR_BIT       then

            Report.Failed  ("unsigned_char is incorrectly defined");

      end if;

      if tst_nul /= C.char'first then
         Report.Failed  ("tst_nul is incorrect");
      end if;

   end;     -- encapsulation

   Report.Result;

end CXB3001;
