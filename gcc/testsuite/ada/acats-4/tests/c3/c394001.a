-- C394001.A
--
--                            Grant of Unlimited Rights
--
--    AdaCore holds unlimited rights in the software and documentation
--    contained herein. Unlimited rights are the same as those granted
--    by the U.S. Government for older parts of the Ada Conformity
--    Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--    By making this public release, AdaCore intends to confer upon all
--    recipients unlimited rights equal to those held by the Ada Conformity
--    Assessment Authority. These rights include rights to use, duplicate,
--    release or disclose the released technical data and computer software
--    in whole or in part, in any manner and for any purpose whatsoever,
--    and to have or permit others to do so.
--
--                                   DISCLAIMER
--
--    ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--    DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--    TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--    DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--    DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--    PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                    Notice
--
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--    This test is based on one submitted by AdaCore; AdaCore retains the
--    copyright on the test.
--*
--  OBJECTIVE:
--      Check that an object of a task interface type can be the prefix of the
--      Terminated and Callable attributes.
--
--      Check that an object of a task interface type can be passed to an
--      abort statement.
--
--  CHANGE HISTORY:
--      20 Oct 2005 HK  Initial Version
--      05 Dec 2005 HK  Add delays to ensure uniform output on different
--                      machines.
--      28 Oct 2007 RLB Corrected objective, made self-testing, renamed
--                      for ACATS 3.0.
--!

with Impdef;
with Report; use Report;

procedure C394001 is

   package Pkg is
      ----------------
      -- Interfaces --
      ----------------

      type Task_Attribute_Iface is task interface;
      procedure Stop (Obj : in out Task_Attribute_Iface) is abstract;

      -------------------------------
      -- Interface implementations --
      -------------------------------

      task type Task_Typ_Task_Attribute_Iface is new
         Task_Attribute_Iface
      with
         entry Stop;
      end Task_Typ_Task_Attribute_Iface;
   end Pkg;

   package body Pkg is

      ------------
      -- Bodies --
      ------------

      task body Task_Typ_Task_Attribute_Iface is
         Terminated : Boolean := False;
      begin
         while not Terminated loop
            accept Stop do
               Terminated := True;
            end Stop;
         end loop;
      end Task_Typ_Task_Attribute_Iface;
   end Pkg;

   use Pkg;

   -----------
   -- Tests --
   -----------

   procedure Check_Value (Value, Expected : Boolean; Attribute : String) is
   begin
      if Value /= Expected then
         Failed (Attribute & " is " & Boolean'Image(Value) & " but " &
                 Boolean'Image(Expected) & " was expected");
      end if;
   end Check_Value;


   procedure Check_Attributes_And_Stop
        (Obj : in out Task_Attribute_Iface'Class) is
   begin
      delay Impdef.Switch_to_New_Task;
      Check_Value (Value => Obj'Callable,
                   Expected => True,
                   Attribute => "Callable");
      Check_Value (Value => Obj'Terminated,
                   Expected => False,
                   Attribute => "Terminated");

      Comment ("  Stop task");
      Obj.Stop;

      delay Impdef.Minimum_Task_Switch;
      Check_Value (Value => Obj'Callable,
                   Expected => False,
                   Attribute => "Callable");
      Check_Value (Value => Obj'Terminated,
                   Expected => True,
                   Attribute => "Terminated");

   end Check_Attributes_And_Stop;


   procedure Check_Attributes_And_Abort
        (Obj : in out Task_Attribute_Iface'Class) is
   begin
      delay Impdef.Switch_to_New_Task;
      Check_Value (Value => Obj'Callable,
                   Expected => True,
                   Attribute => "Callable");
      Check_Value (Value => Obj'Terminated,
                   Expected => False,
                   Attribute => "Terminated");

      Comment ("  Abort task");
      abort Obj;

      delay Impdef.Minimum_Task_Switch;
      Check_Value (Value => Obj'Callable,
                   Expected => False,
                   Attribute => "Callable");
      Check_Value (Value => Obj'Terminated,
                   Expected => True,
                   Attribute => "Terminated");
   end Check_Attributes_And_Abort;

   --  Local variables

   TTTAI1 : Task_Typ_Task_Attribute_Iface;
   TTTAI2 : Task_Typ_Task_Attribute_Iface;

begin
   Test ("C394001", "Check that an object of a task interface type can be " &
                    "the prefix of the Terminated and Callable attributes " &
                    "and be passed to an abort statement");

   Check_Attributes_And_Stop  (TTTAI1);
   Check_Attributes_And_Abort (TTTAI2);

   Result;
end C394001;
