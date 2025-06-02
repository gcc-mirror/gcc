-- CXAJ001.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--      Check that Ada.Locales is supported.
--
-- TEST DESCRIPTION:
--
--      We ask for the Country and Language, and verify that they're in the
--      expected range.
--
--      We could have checked that the code matched a value specified in
--      Impdef, but that would make the test suite less more fragile, as the
--      test would then fail if run on a machine with some other country or
--      language. That would be annoying for organizations that have offices
--      and/or machines in multiple locations. The check doesn't seem valuable
--      enough for the downside, thus we just use comments to report the
--      returned codes.
--
-- CHANGE HISTORY:
--      03 Dec 13   RLB    Initial version.
--      20 Mar 14   RLB    Readied to issue; minor changes to comments.
--!
with Ada.Locales;
with Report;
with Ada.Exceptions;
procedure CXAJ001 is

   Country  : Ada.Locales.Country_Code;

   Language : Ada.Locales.Language_Code;

begin
   Report.Test ("CXAJ001", "Check that Ada.Locales is supported");

   begin
      Country := Ada.Locales.Country;
      Report.Comment ("Current country code is " & Country(1) & Country(2));
      if Country(1) not in 'A' .. 'Z' or else
         Country(2) not in 'A' .. 'Z' then
         Report.Failed ("Country code not in expected range");
      --else OK
      end if;
   exception
      when Err:others =>
         Report.Failed ("Unexpected exception from Country - " &
            Ada.Exceptions.Exception_Information (Err));
   end;

   begin
      Language := Ada.Locales.Language;
      Report.Comment ("Current language code is " &
                      Language(1) & Language(2) & Language(3));
      if Language(1) not in 'a' .. 'z' or else
         Language(2) not in 'a' .. 'z' or else
         Language(3) not in 'a' .. 'z' then
         Report.Failed ("Language code not in expected range");
      --else OK
      end if;
   exception
      when Err:others =>
         Report.Failed ("Unexpected exception from Language - " &
            Ada.Exceptions.Exception_Information (Err));
   end;

   Report.Result;

end CXAJ001;

