--
-- CD72A01.A
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
--      Check that the package System.Address_To_Access_Conversions may be
--      instantiated for various simple types.
--
--      Check that To_Pointer and To_Address are inverse operations.
--
--      Check that To_Pointer(X'Address) equals X'Unchecked_Access for an
--      X that allows Unchecked_Access.
--
--      Check that To_Pointer(Null_Address) returns null.
--
-- TEST DESCRIPTION:
--      This test checks that the semantics provided in
--      Address_To_Access_Conversions are present and operate
--      within expectations (to the best extent possible in a portable
--      implementation independent fashion).
--
--      The functions Address_To_Hex and Hex_To_Address test the invertability
--      of the To_Integer and To_Address functions, along with a great deal
--      of optimizer chaff and protection from the fact that type
--      Storage_Elements.Integer_Address may be either a modular or a signed
--      integer type.
--
--      This test has some interesting usage paradigms in that users
--      occasionally want to store address information in a transportable
--      fashion, and often resort to some textual representation of values.  
--
-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Systems Programming Annex (C):
--        this test must execute and report PASSED.
--
--      For implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-C RQMT", in which case it may be graded as inapplicable.
--        Otherwise, the test must execute and report PASSED.
--
-- CHANGE HISTORY:
--      13 JUL 95   SAIC   Initial version (CD72001)
--      08 FEB 96   SAIC   Revised (split) version for 2.1
--      07 MAY 96   SAIC   Additional subtest added for 2.1
--      16 FEB 98   EDS    Modified documentation.
--!

with Report;
with Impdef;
with FD72A00;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
procedure CD72A01 is
  use System;
  use FD72A00;

  package Number_ATAC is
      new System.Address_To_Access_Conversions(Number); -- ANX-C RQMT

  use type Number_ATAC.Object_Pointer;

  type Data is record
    One, Two: aliased Number;
  end record;

  package Data_ATAC is
      new System.Address_To_Access_Conversions(Data);   -- ANX-C RQMT

  use type Data_ATAC.Object_Pointer;

  type Test_Cases is ( Addr_Type, Record_Type );

  type Naive_Dynamic_String is access String;

  type String_Store is array(Test_Cases) of Naive_Dynamic_String;

  The_Strings : String_Store;

  -- create several aliased objects with distinct values

  My_Number : aliased Number := Number'First;
  My_Data   : aliased Data   := (Number'First,Number'Last);

   use type System.Storage_Elements.Integer_Address;

begin  -- Main test procedure.

   Report.Test ("CD72A01", "Check package " &
                            "System.Address_To_Access_Conversions " &
                            "for simple types" );
  
    -- take several pointer objects, convert them to addresses, and store
    -- the address as a hexadecimal representation for later reconversion

    The_Strings(Addr_Type) := new String'(
      Address_To_Hex(Number_ATAC.To_Address(My_Number'Access)) );

    The_Strings(Record_Type) := new String'(
      Address_To_Hex(Data_ATAC.To_Address(My_Data'Access)) );

    -- now, reconvert the hexadecimal address values back to pointers,
    -- and check that the dereferenced pointer still designates the
    -- value placed at that location.  The use of the intermediate
    -- string representation should foil even the cleverest of optimizers

    if Number_ATAC.To_Pointer(
                             Hex_To_Address(The_Strings(Addr_Type))).all
       /= Number'First then
      Report.Failed("Number reconversion");
    end if;

    if Data_ATAC.To_Pointer(Hex_To_Address(The_Strings(Record_Type))).all
       /= (Number'First,Number'Last) then
      Report.Failed("Data reconversion");
    end if;

    -- check that the resulting values are equal to the 'Unchecked_Access
    -- of the value

    if Number_ATAC.To_Pointer(
                             Hex_To_Address(The_Strings(Addr_Type)))
       /= My_Number'Unchecked_Access then
      Report.Failed("Number Unchecked_Access");
    end if;

    if Data_ATAC.To_Pointer(Hex_To_Address(The_Strings(Record_Type)))
       /= My_Data'Unchecked_Access then
      Report.Failed("Data Unchecked_Access");
    end if;

   if Number_ATAC.To_Pointer(System.Null_Address) /= null then
     Report.Failed("To_Pointer(Null_Address) /= null");
   end if;

   if Number_ATAC.To_Address(null) /= System.Null_Address then
     Report.Failed("To_Address(null) /= Null_Address");
   end if;

   Report.Result;

end CD72A01;
