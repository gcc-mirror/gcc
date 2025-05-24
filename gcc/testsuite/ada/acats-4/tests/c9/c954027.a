-- C954027.A
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
--      Check requeing to a procedure of a synchronized interface.
--      Check that the the interface can be implemented by a task or a
--      protected object.
--
-- TEST DESCRIPTION:
--      This test models a dispatcher that forwards a service (adding
--      a value to a variable) to servers that register and implement
--      the service.  In the test, two servers are created: one is a
--      task, and one a protected object. The added value is different
--      for each server in order to identify which one was executed
--
-- CHANGE HISTORY:
--      06 Jan 12   JPR     ACVC 3.0.
--      14 Apr 14   RLB     Renamed to add to ACATS 4.0; corrected various
--                          comments.
--
--!

with Report;
procedure C954027 is
   package Adder is
      type Implementation is synchronized interface;
      procedure Do_It (Self  : in out Implementation;
		       Var   : in out Integer)
	is abstract with Synchronization => By_Entry;
   end Adder;
   type Adder_Implementer is access all Adder.Implementation'Class;
   type Adder_Array is array (Natural range <>) of Adder_Implementer;

   protected Dispatcher is
      procedure Register (An_Adder : Adder_Implementer);
      entry Do_It (Var   : in out Integer);
   private
      Registered : Natural := 0;
      Servers    : Adder_Array (0..1);
      Current    : Natural := 0;
   end Dispatcher;

   protected body Dispatcher is
      procedure Register (An_Adder : Adder_Implementer) is
      begin
	 Servers (Registered) := An_Adder;
	 Registered := Registered + 1;
      end Register;

      entry Do_It (Var	 : in out Integer) when True is
	 function Next_Server return Natural is
	 begin
	    return Result : constant Natural := Current do
	      Current := (Current + 1) mod Registered;
	    end return;
	 end Next_Server;

      begin
	 requeue Servers (Next_Server).Do_It;
      end Do_It;
   end Dispatcher;

   --
   -- Task implementation
   --
   task type Task_Adder is new Adder.Implementation with
     entry Registered;
     entry Do_It (Var : in out Integer);
   end Task_Adder;

   task body Task_Adder is
   begin
      Dispatcher.Register (Task_Adder'Unchecked_Access);
      accept Registered;

      loop
	 select
	    accept Do_It (Var : in out Integer) do
	       Var := Var + 1;
	    end Do_It;
	 or
	    terminate;
	 end select;
      end loop;
   end Task_Adder;

   --
   -- PO implementation
   --
   protected type PO_Adder is new Adder.Implementation with
     entry Do_It (Var : in out Integer);
   end PO_Adder;

   protected body PO_Adder is
     entry Do_It (Var : in out Integer) when True is
     begin
	Var := Var + 2;
     end Do_It;
   end PO_Adder;

   TA : Task_Adder;
   PA : aliased PO_Adder;
   V : Integer := 0;
begin
   Report.Test ("C954027",
                 "Check that an entry call can be requeued on a procedure" &
                  " of a synchronized interface, implemented by" &
		  " a task or a protected object");

   -- Make sure the task has registered before registering the PO
   TA.Registered;
   Dispatcher.Register (PA'Access);

   Dispatcher.Do_It (V);
   if V /= 1 then
      Report.Failed ("Incorrect result with requeue to task");
   end if;

   Dispatcher.Do_It (V);
   if V /= 3 then
      Report.Failed ("Incorrect result with requeue to PO");
   end if;

   Report.Result;
exception
   when others =>
      Report.Failed ("Exception in main");
end C954027;
