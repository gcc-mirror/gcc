-- CD72A02.A
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
--      instantiated for various composite types.
--
--      Check that To_Pointer and To_Address are inverse operations.
--
--      Check that To_Pointer(X'Address) equals X'Unchecked_Access for an
--      X that allows Unchecked_Access.
--
--      Check that To_Pointer(Null_Address) returns null.
--
-- TEST DESCRIPTION:
--      This test is identical to CD72A01 with the exception that it tests
--      the composite types where CD72A01 tests "simple" types.
--
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
--
-- CHANGE HISTORY:
--      13 JUL 95   SAIC   Initial version (CD72001)
--      08 FEB 96   SAIC   Split from CD72001 by reviewer request for 2.1
--      12 NOV 96   SAIC   Corrected typo in RM ref
--      16 FEB 98   EDS    Modified documentation.
--      22 JAN 02   RLB    Corrected test description.
--!

with Report;
with Impdef;
with FD72A00;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
procedure CD72A02 is
  use System;
  use FD72A00;

   type Tagged_Record is tagged record
      Value : Natural;
    end record;

    package Class_ATAC is
      new System.Address_To_Access_Conversions(Tagged_Record'Class);
                                                        -- ANX-C RQMT

  use type Class_ATAC.Object_Pointer;

    task type TC_Task_Type is
      entry E;
      entry F;
    end TC_Task_Type;

    package Task_ATAC is
            new System.Address_To_Access_Conversions(TC_Task_Type);
                                                        -- ANX-C RQMT

  use type Task_ATAC.Object_Pointer;

    task body TC_Task_Type is
    begin
      select
        accept E;
      or
        accept F;
          Report.Failed("Task rendezvoused on wrong path");
      end select;
    end TC_Task_Type;

    protected type TC_Protec is
      procedure E;
      procedure F;
    private
      Visited : Boolean := False;
    end TC_Protec;

    package Protected_ATAC is
            new System.Address_To_Access_Conversions(TC_Protec);
                                                        -- ANX-C RQMT

  use type Protected_ATAC.Object_Pointer;

    protected body TC_Protec is
      procedure E is
      begin
        Visited := True;
      end E;
      procedure F is
      begin
        if not Visited then
          Report.Failed("Protected Object took wrong path");
        end if;
      end F;
    end TC_Protec;

    type Test_Cases is ( Tagged_Type, Task_Type,  Protected_Type );

    type Naive_Dynamic_String is access String;

    type String_Store is array(Test_Cases) of Naive_Dynamic_String;

    The_Strings : String_Store;

    -- create several aliased objects with distinct values

    My_Rec  : aliased Tagged_Record := (Value => Natural'Last);
    My_Task : aliased TC_Task_Type;
    My_Prot : aliased TC_Protec;

    use type System.Storage_Elements.Integer_Address;

begin  -- Main test procedure.

   Report.Test ("CD72A02", "Check package " &
                            "System.Address_To_Access_Conversions " &
                            "for composite types" );

    -- take several pointer objects, convert them to addresses, and store
    -- the address as a hexadecimal representation for later reconversion

    The_Strings(Tagged_Type) := new String'(
      Address_To_Hex(Class_ATAC.To_Address(My_Rec'Access)) );

    The_Strings(Task_Type) := new String'(
      Address_To_Hex(Task_ATAC.To_Address(My_Task'Access)) );

    The_Strings(Protected_Type) := new String'(
      Address_To_Hex(Protected_ATAC.To_Address(My_Prot'Access)) );

    -- now, reconvert the hexadecimal address values back to pointers,
    -- and check that the dereferenced pointer still designates the
    -- value placed at that location.  The use of the intermediate
    -- string representation should foil even the cleverest of optimizers

    if Tagged_Record(Class_ATAC.To_Pointer(
                              Hex_To_Address(The_Strings(Tagged_Type))).all)
       /= Tagged_Record'(Value => Natural'Last) then
      Report.Failed("Tagged_Record reconversion");
    end if;

    Task_ATAC.To_Pointer(Hex_To_Address(The_Strings(Task_Type))).E;

    begin
      select        -- allow for task to have completed.
        My_Task.F;  -- should not happen, will call Report.Fail in task
      else
        null;       -- expected case, "Report.Pass;"
      end select;
    exception
      when Tasking_Error => null;  -- task terminated, which is OK
    end;

    Protected_ATAC.To_Pointer(
                           Hex_To_Address(The_Strings(Protected_Type))).E;
    My_Prot.F;    -- checks that call to E occurred


    -- check that the resulting values are equal to the 'Unchecked_Access
    -- of the value

    if Class_ATAC.To_Pointer(Hex_To_Address(The_Strings(Tagged_Type)))
       /= My_Rec'Unchecked_Access then
      Report.Failed("Tagged_Record Unchecked_Access");
    end if;

    if Task_ATAC.To_Pointer(Hex_To_Address(The_Strings(Task_Type)))
       /= My_Task'Unchecked_Access then
      Report.Failed("Task Unchecked_Access");
    end if;

    if Protected_ATAC.To_Pointer(
                           Hex_To_Address(The_Strings(Protected_Type)))
       /= My_Prot'Unchecked_Access then
      Report.Failed("Protected Unchecked_Access");
    end if;

  Report.Result;

end CD72A02;
