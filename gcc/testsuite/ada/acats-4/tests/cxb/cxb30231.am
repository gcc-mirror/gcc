--  CXB30231.AM
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
--
-- OBJECTIVE:
--    Check correct operation of unchecked unions:
--      1) Calling a C routine to process an unchecked_union results in
--         same result as produced by corresponding routine in Ada
--      2) Predefined equality should raise Program_Error if either object
--         in the comparison does not have inferrable_discriminants
--    Part 1: Pragma Unchecked_Union.

-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      packages Interfaces.C and convention C. If an
--      implementation provides package Interfaces.C and convention C,
--      this test must compile, execute, and report "PASSED".
--
--      Exception: This test presumes that the Implementation Advice in
--      B.3 is followed. In particular, the test assumes that B.3(69/2) and
--      B.3(70) are followed for record and array types. If following
--      this Advice is impractical, this test is not applicable to the
--      implementation. The implementer needs to advise their ACAL and the
--      ACAA if this is the case as it cannot be detected by inspecting
--      the test results; a test run that does not report PASSED or otherwise
--      fails will be graded as FAILED unless the ACAL and ACAA is advised
--      of the issue.
--
-- SPECIAL REQUIREMENTS:
--      The files CXB30230.C must be compiled with a C
--      compiler. Implementation dialects of C may require alteration of
--      the C program syntax (see individual C files).
--
--      Note that the compiled C code must be bound with the compiled Ada
--      code to create an executable image. An implementation must provide
--      the necessary commands to accomplish this.
--
--      Note that the C code included in CXB30230.C conforms
--      to ANSI-C. Modifications to these files may be required for other
--      C compilers. An implementation must provide the necessary
--      modifications to satisfy the function requirements.
--
-- TEST FILES:
--      This test consists of the following files:
--         CXB30230.C
--      -> CXB30231.AM
--
--
--  CHANGE HISTORY:
--    07 Jun 2015 BJM Created ACATS test.
--    06 Sep 2015 BJM Redid test to avoid erroneous code.
--    25 Nov 2015 RLB Readied for release; removed named aggregates (as we
--                    don't want to assume AI12-0174-1 is implemented);
--                    renamed, used interface names from IMPDEF, added
--                    Applicability Criteria.
--    09 Dec 2015 RLB Removed pointer case, which used a non-language-defined
--                    function and unreliable assumptions.
--    28 Apr 2016 RLB Added missing N/A => ERROR indications.

--!

with Interfaces.C.Strings; use Interfaces;              -- N/A => ERROR.
with Ada.Text_IO;
with Report, ImpDef;

procedure CXB30231 is

   package C_Float_IO is new Ada.Text_IO.Float_IO (Num => C.C_float);

   type Data_Kind is (C_int, C_char, C_string, C_float);
   pragma Convention (C, Data_Kind);                    -- N/A => ERROR.

   for Data_Kind use (C_int => 0,
                      C_char => 1,
                      C_string => 2,
                      C_float => 3);

   type C_Variant (Format : Data_Kind := C_int) is
      record
         case Format is
            when C_int =>
               int_Val : C.int;

            when C_char =>
               char_Val : C.char;

            when C_string =>
               str_Val : C.Strings.chars_ptr;

            when C_float =>
               float_Val : C.C_float;

         end case;
      end record;
   pragma Convention (C, C_Variant);
   pragma Unchecked_Union (C_Variant);

   type C_Multi_Type is
      record
         Kind    : Data_Kind;
         Variant : C_Variant;
      end record;
     pragma Convention (C, C_Multi_Type);

   type C_Multi_Type_Access is access all C_Multi_Type;

   procedure C_Format_Variant (Item : C_Multi_Type_Access;
                               Result : access C.char);
   pragma Import (C, C_Format_Variant,
                  External_Name => ImpDef.CXB30230_External_Name);

   function Ada_Format_Variant (Item : C_Multi_Type) return String is
   begin
      case Item.Kind is

         when C_int =>
            return "Type: Integer, Val:" & C.int'Image (Item.Variant.int_Val);

         when C_char =>
            return "Type: Character, Val: " &
              C.char'Image (Item.Variant.char_Val);

         when C_string =>
            return "Type: String, Val: " &
              C.Strings.Value (Item.Variant.str_Val);

         when C_float =>
            declare
               Result : String (1 .. 4);
            begin

               C_Float_IO.Put (To => Result,
                               Item => Item.Variant.float_Val,
                               Aft  => 2,
                               Exp  => 0);

               return "Type: Real, Val: " & Result;
            end;

      end case;
   end Ada_Format_Variant;

   C_Char_Array      : aliased C.char_array := C.To_C ("Hello World");

   Var1, Var2 : aliased C_Multi_Type;

begin
   Report.Test ("CXB3023",
                "Check correct operation of unchecked unions");

   Var1 := (Kind => C_int, Variant => (C_int, 10));

   declare
      Expected_Output : constant String := "Type: Integer, Val: 10";
      Ada_Output_String : constant String := Ada_Format_Variant (Var1);
      C_Output_String   : aliased C.char_array (1 .. 80);
   begin

      if Ada_Output_String /= Expected_Output then

         Report.Failed
           (Descr =>
              "1) Unexpected values (" & Ada_Output_String &
              ") extracted from unchecked union. Expected: "
            & Expected_Output);
      end if;

      -- Call a C routine to generate the same output string
      C_Format_Variant (Var1'Access,
                        C_Output_String (C_Output_String'First)'Access);

      if Ada_Output_String /= C.To_Ada (C_Output_String) then

         Report.Failed
           (Descr => "2) Ada interpretation (" & Ada_Output_String & ") " &
              "of union differs from C (" & C.To_Ada (C_Output_String) & ')');
      end if;

   end;

   Var1 := (Kind => C_char, Variant => (C_char, '%'));

   declare
      Expected_Output   : constant String := "Type: Character, Val: '%'";
      Ada_Output_String : constant String := Ada_Format_Variant (Var1);
      C_Output_String   : aliased C.char_array (1 .. 80);
   begin

      if Ada_Output_String /= Expected_Output then

         Report.Failed
           (Descr =>
              "3) Unexpected values (" & Ada_Output_String &
              ") extracted from unchecked union. Expected: " &
              Expected_Output);
      end if;

      -- Call a C routine to generate the same output string
      C_Format_Variant (Var1'Access,
                        C_Output_String (C_Output_String'First)'Access);

      if Ada_Output_String /= C.To_Ada (C_Output_String) then

         Report.Failed
           (Descr => "4) Ada interpretation (" & Ada_Output_String & ") " &
              "of union differs from C (" & C.To_Ada (C_Output_String) & ')');
      end if;

   end;

   Var1 := (Kind => C_string,
               Variant =>
                 (C_string,
                  C.Strings.To_Chars_Ptr
                    (C_Char_Array'Unchecked_Access)));

   declare
      Expected_Output   : constant String := "Type: String, Val: Hello World";
      Ada_Output_String : constant String := Ada_Format_Variant (Var1);
      C_Output_String   : aliased C.char_array (1 .. 80);
   begin

      if Ada_Output_String /= Expected_Output then

         Report.Failed
           (Descr =>
              "5) Unexpected values (" & Ada_Output_String &
              ") extracted from unchecked union. Expected: " &
              Expected_Output);
      end if;

      -- Call a C routine to generate the same output string
      C_Format_Variant (Var1'Access,
                        C_Output_String (C_Output_String'First)'Access);

      if Ada_Output_String /= C.To_Ada (C_Output_String) then

         Report.Failed
           (Descr => "6) Ada interpretation (" & Ada_Output_String & ") " &
              "of union differs from C (" & C.To_Ada (C_Output_String) & ')');
      end if;

   end;

   Var1 := (Kind => C_float, Variant => (C_float, 3.14));

   declare
      Expected_Output   : constant String := "Type: Real, Val: 3.14";
      Ada_Output_String : constant String := Ada_Format_Variant (Var1);
      C_Output_String   : aliased C.char_array (1 .. 80);
   begin

      if Ada_Output_String /= Expected_Output then

         Report.Failed
           (Descr =>
              "7) Unexpected values (" & Ada_Output_String &
              ") extracted from unchecked union. Expected: " &
              Expected_Output);
      end if;

      -- Call a C routine to generate the same output string
      C_Format_Variant (Var1'Access,
                        C_Output_String (C_Output_String'First)'Access);

      if Ada_Output_String /= C.To_Ada (C_Output_String) then

         Report.Failed
           (Descr => "8) Ada interpretation (" & Ada_Output_String & ") " &
              "of union differs from C (" & C.To_Ada (C_Output_String) & ')');
      end if;

      Var2 := Var1;

      begin
         if Var1 = Var2 then
            Report.Failed
              (Descr =>
                 "11) Var1 and Var2 lack inferrable discriminants, so " &
                 "Program_Error should have been raised for predefined " &
                 "equality test");
         else
            Report.Failed
              (Descr =>
                 "12) Var1 and Var2 are same, but equality is False");
         end if;
      exception
         when Program_Error =>
            null; -- Expected exception.
      end;

   end;

   Report.Result;

end CXB30231;

