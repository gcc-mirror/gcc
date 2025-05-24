-- C760007.A
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
--      Check that Adjust is called for the execution of a return
--      statement for a function returning a result of a (non-limited)
--      controlled type.
--
--      Check that Adjust is called when evaluating an aggregate
--      component association for a controlled component.
--
--      Check that Adjust is called for the assignment of the ancestor
--      expression of an extension aggregate when the type of the
--      aggregate is controlled.
--
-- TEST DESCRIPTION:
--      A type is derived from Ada.Finalization.Controlled; the dispatching
--      procedure Adjust is defined for the new type.  Structures and
--      subprograms to model the test objectives are used to check that
--      Adjust is called at the right time.  For the sake of simplicity,
--      globally accessible data is used to check that the calls are made.
--
--
-- CHANGE HISTORY:
--      06 DEC 94   SAIC    ACVC 2.0
--      14 OCT 95   SAIC    Update and repair for ACVC 2.0.1
--      05 APR 96   SAIC    Add RM reference
--      06 NOV 96   SAIC    Reduce adjust requirement
--      25 NOV 97   EDS     Allowed zero calls to adjust at line 144
--!

---------------------------------------------------------------- C760007_0

with Ada.Finalization;
package C760007_0 is

  type Controlled is new Ada.Finalization.Controlled with record
    TC_ID : Natural := Natural'Last;
  end record;
  procedure Adjust( Object: in out Controlled );

  type Structure is record
    Controlled_Component : Controlled;
  end record;

  type Child is new Controlled with record
    TC_XX : Natural := Natural'Last;
  end record;
  procedure Adjust( Object: in out Child );

  Adjust_Count       : Natural := 0;
  Child_Adjust_Count : Natural := 0;

end C760007_0;
 
package body C760007_0 is

  procedure Adjust( Object: in out Controlled ) is
  begin
    Adjust_Count := Adjust_Count +1;
  end Adjust;

  procedure Adjust( Object: in out Child ) is
  begin
    Child_Adjust_Count := Child_Adjust_Count +1;
  end Adjust;

end C760007_0;

------------------------------------------------------------------ C760007

with Report;
with C760007_0;
procedure C760007 is

  procedure Check_Adjust_Count(Message: String;
                               Min: Natural := 1;
                               Max: Natural := 2) is
  begin

     -- in order to allow for the anonymous objects referred to in
     -- the reference manual, the check for calls to Adjust must be
     -- in a range.  This number must then be further adjusted
     -- to allow for the optimization that does not call for an adjust
     -- of an aggregate initial value built directly in the object

     if C760007_0.Adjust_Count not in Min..Max then
       Report.Failed(Message
                   & " = " & Natural'Image(C760007_0.Adjust_Count));
     end if;
     C760007_0.Adjust_Count := 0;
  end Check_Adjust_Count;

  procedure Check_Child_Adjust_Count(Message: String;
                                     Min: Natural := 1;
                                     Max: Natural := 2) is
  begin
     -- ditto above

     if C760007_0.Child_Adjust_Count not in Min..Max then
       Report.Failed(Message
                   & " = " & Natural'Image(C760007_0.Child_Adjust_Count));
     end if;
     C760007_0.Child_Adjust_Count := 0;
  end Check_Child_Adjust_Count;

  Object : C760007_0.Controlled;

--      Check that Adjust is called for the execution of a return
--      statement for a function returning a result of a (non-limited)
--      controlled type or a result of a noncontrolled type with
--      controlled components.

  procedure Subtest_1 is
    function Create return C760007_0.Controlled is
      New_Object : C760007_0.Controlled;
    begin
      return New_Object;
    end Create;

    procedure Examine( Thing : in C760007_0.Controlled ) is
    begin
      Check_Adjust_Count("Function call passed as parameter",0);
    end Examine;

  begin
    -- this assignment must call Adjust:
    --   1: on the value resulting from the function
    --      ** unless this is optimized out by building the result directly
    --         in the target object.
    --   2: on Object once it's been assigned
    -- may call adjust
    --   1: for a anonymous object created in the evaluation of the function
    --   2: for a anonymous object created in the assignment operation

    Object := Create;

    Check_Adjust_Count("Function call",1,4);

    Examine( Create );

  end Subtest_1;

--      Check that Adjust is called when evaluating an aggregate
--      component association for a controlled component.

  procedure Subtest_2 is
    S : C760007_0.Structure;

    procedure Examine( Thing : in C760007_0.Structure ) is
    begin
      Check_Adjust_Count("Aggregate passed as parameter");
    end Examine;

  begin
    -- this assignment must call Adjust:
    --   1: on the value resulting from the aggregate
    --      ** unless this is optimized out by building the result directly
    --         in the target object.
    --   2: on Object once it's been assigned
    -- may call adjust
    --   1: for a anonymous object created in the evaluation of the aggregate
    --   2: for a anonymous object created in the assignment operation
    S := ( Controlled_Component => Object );
    Check_Adjust_Count("Aggregate and Assignment", 1, 4);

    Examine( C760007_0.Structure'(Controlled_Component => Object) );
  end Subtest_2;

--      Check that Adjust is called for the assignment of the ancestor
--      expression of an extension aggregate when the type of the
--      aggregate is controlled.

  procedure Subtest_3 is
    Bambino : C760007_0.Child;

    procedure Examine( Thing : in C760007_0.Child ) is
    begin
      Check_Adjust_Count("Extension aggregate as parameter (ancestor)", 0, 2);
      Check_Child_Adjust_Count("Extension aggregate as parameter", 0, 4);
    end Examine;

  begin
    -- implementation permissions make all of the following calls to adjust
    -- optional:
    -- these assignments may call Adjust:
    --   1: on the value resulting from the aggregate
    --   2: on Object once it's been assigned
    --   3: for a anonymous object created in the evaluation of the aggregate
    --   4: for a anonymous object created in the assignment operation
    Bambino := ( Object with TC_XX => 10 );
    Check_Adjust_Count("Ancestor (expression) part of aggregate", 0, 2);
    Check_Child_Adjust_Count("Child aggregate assignment 1", 0, 4 );

    Bambino := ( C760007_0.Controlled with TC_XX => 11 );
    Check_Adjust_Count("Ancestor (subtype_mark) part of aggregate", 0, 2);
    Check_Child_Adjust_Count("Child aggregate assignment 2", 0, 4 );

    Examine( ( Object with TC_XX => 21 ) );

    Examine( ( C760007_0.Controlled with TC_XX => 37 ) );

  end Subtest_3;

begin  -- Main test procedure.

  Report.Test ("C760007", "Check that Adjust is called for the " &
                          "execution of a return statement for a " &
                          "function returning a result containing a " &
                          "controlled type.  Check that Adjust is " &
                          "called when evaluating an aggregate " &
                          "component association for a controlled " &
                          "component.  " &
                          "Check that Adjust is called for the " &
                          "assignment of the ancestor expression of an " &
                          "extension aggregate when the type of the " &
                          "aggregate is controlled" );

  Subtest_1;
  Subtest_2;
  Subtest_3;

  Report.Result;

end C760007;
