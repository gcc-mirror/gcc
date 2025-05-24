-- CXAI009.A
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
--      Check that an implementation supports the functionality defined
--      in package Ada.Containers.Indefinite_Holders.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Containers.Indefinite_Holders.
--      Each of the subprograms is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      30 Oct 13   JAC     Initial pre-release version.
--      31 Jan 14   JAC     Second pre-release version.
--      27 Mar 14   RLB     Created ACATS 4.0 version, renamed test.
--      28 Mar 14   RLB     Revised Reference/Constant_Reference test case
--                          to be more similar to likely usage.
--      31 Mar 14   RLB     Corrected generalized reference case.
--!
with Ada.Containers.Indefinite_Holders;
with Report;

procedure CXAI009 is

   package My_Indefinite_Holders is new
     Ada.Containers.Indefinite_Holders
       (Element_Type => String); -- Default =

   My_Holder_1 : My_Indefinite_Holders.Holder;
   My_Holder_2 : My_Indefinite_Holders.Holder;

   Animal_1 : constant String := "Rabbit";
   Animal_2 : constant String := "Monkey";
   Animal_3 : constant String := "Ostrich";

   procedure Tampering_Check
     (Container : in out My_Indefinite_Holders.Holder;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Container := My_Indefinite_Holders.To_Holder (Animal_1);

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type My_Indefinite_Holders.Holder;

begin

   Report.Test
     ("CXAI009",
      "Check that an implementation supports the functionality defined in " &
      "package Ada.Containers.Indefinite_Holders");


   -- Test empty using Empty_Holder, Is_Empty and Length

   if My_Holder_1 /= My_Indefinite_Holders.Empty_Holder then

      Report.Failed ("Not initially empty");

   end if;

   if not My_Holder_1.Is_Empty then

      Report.Failed ("Not initially empty");

   end if;


   -- Test To_Holder, Element and Query_Element

   My_Holder_1 := My_Indefinite_Holders.To_Holder (Animal_1);

   if My_Holder_1.Element /= Animal_1 then

      Report.Failed ("Wrong element after after assignmnet");

   end if;

   declare

      procedure My_Query (Element : in String) is
      begin

         Tampering_Check
           (Container => My_Holder_1,
            Where     => "Query_Element");

         if Element /= Animal_1 then

            Report.Failed ("Mismatch between element and what was assigned");

         end if;

      end My_Query;

   begin

      My_Holder_1.Query_Element (Process  => My_Query'Access);

   end;


   My_Holder_2 := My_Indefinite_Holders.To_Holder (Animal_1);


   -- Test equality

   if My_Holder_1 /= My_Holder_2 then

      Report.Failed ("Holders not equal");

   end if;


   -- Test Replace_Element and Update_Element

   declare

      procedure My_Update (Element : in out String) is
      begin

         Tampering_Check
           (Container => My_Holder_2,
            Where     => "Update_Element");

         Element := Animal_2;
         -- New element has to have the same length as the old one otherwise get
         -- CONSTRAINT_ERROR : length check failed

      end My_Update;

   begin

      My_Holder_1.Replace_Element (New_Item => Animal_2);
      -- New element may have a different length from the old one

      My_Holder_2.Update_Element (Process  => My_Update'Access);

   end;

   if My_Holder_1 /= My_Holder_2 then

      Report.Failed ("Modified Holders not equal");

   end if;


   -- Test Clear and inequality

   My_Holder_1.Clear;

   if not My_Holder_1.Is_Empty then

      Report.Failed ("Failed to clear");

   end if;

   -- Check can replace by an animal of a different length

   My_Holder_1 := My_Indefinite_Holders.To_Holder (Animal_3);

   if My_Holder_1 = My_Holder_2 then

      Report.Failed ("Different Holders equal");

   end if;


   -- Test Move

   My_Holder_2.Clear;

   My_Holder_2 := My_Indefinite_Holders.To_Holder (Animal_1);

   My_Holder_1.Move (Source => My_Holder_2);

   if not My_Holder_2.Is_Empty then

      Report.Failed ("Moved source not empty");

   end if;

   if My_Holder_1.Element /= Animal_1 then

      Report.Failed ("Wrong element after after Assign");

   end if;


   -- Test Assign and Copy

   My_Holder_2.Assign (Source => My_Holder_1);

   if My_Holder_2.Element /= Animal_1 then

      Report.Failed ("Wrong element after after Assign");

   end if;

   if My_Holder_1.Element /= Animal_1 then

      Report.Failed ("Source element not left alone by Assign");

   end if;

   My_Holder_2.Clear;

   My_Holder_2 := My_Indefinite_Holders.Copy (Source => My_Holder_1);

   if My_Holder_2.Element /= Animal_1 then

      Report.Failed ("Wrong element after after Copy");

   end if;

   if My_Holder_1.Element /= Animal_1 then

      Report.Failed ("Source element not left alone by Copy");

   end if;


   -- Test Constant_Reference and Reference explicitly in a way
   -- that shows a possible use.

   declare
      procedure Test (Value    : in String;
                      Expected : in String;
                      Test_Case: in String) is
      begin
         Tampering_Check
           (Container => My_Holder_1,
            Where     => Test_Case);
            -- The tampering check here prevents the
            -- Value parameter to this subprogram from
            -- disappearing (if passed by-reference) while
            -- it is still in use.

         if Value /= Expected then
            Report.Failed ("Wrong value for " & Test_Case);
         end if;
      end Test;

   begin
      -- Normal call:
      Test (Value    =>
               My_Indefinite_Holders.Constant_Reference(My_Holder_1).Element.all,
            Expected => Animal_1,
            Test_Case=> "Constant_Reference normal");

      Test (Value    =>
               My_Indefinite_Holders.Reference(My_Holder_1).Element.all,
            Expected => Animal_1,
            Test_Case=> "Reference normal");

      -- Prefix call with all components explicit:
      Test (Value    => My_Holder_1.Constant_Reference.Element.all,
            Expected => Animal_1,
            Test_Case=> "Constant_Reference prefix");

      Test (Value    => My_Holder_1.Reference.Element.all,
            Expected => Animal_1,
            Test_Case=> "Reference prefix");

      -- Using generalized reference:

      Test (Value    => My_Holder_1.Constant_Reference,
            Expected => Animal_1,
            Test_Case=> "Constant_Reference generalized");

      Test (Value    => My_Holder_1.Reference,
            Expected => Animal_1,
            Test_Case=> "Reference generalized");
         -- This case is the way that we expect that a holder reference
         -- will be used.

   end;

   Report.Result;

end CXAI009;
