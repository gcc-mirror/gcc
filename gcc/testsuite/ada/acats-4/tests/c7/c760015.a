-- C760015.A
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
--    Check that a controlled type can be declared within a subprogram,
--    and that an overridden Initialize or Finalize is called appropriately
--    and can access local variables to the subprogram.
--
-- TEST DESCRIPTION:
--    We declare a number of subprograms that contain controlled types with
--    overridden Initialize and Finalize. We check that Initialize and Finalize
--    are called appropriately when the subprogram is called and left, whether
--    it is left normally or via an exception.
--
--    This was not allowed in Ada 95; AI95-0344-1 makes it possible for
--    newer versions of Ada. The driving reason for that was to allow
--    container generics to be instantiated within subprograms, which really
--    boils down to allowing controlled types to be declared within
--    subprograms. If a container package has instance data in the package
--    body, accessing that data from an Initialize or Finalize would be the
--    same as what we're testing here (without the complication of an
--    enclosing generic).
--
--    This test also models a last-chance handler for a subprogram.
--
-- CHANGE HISTORY:
--    09 Jan 2015   RLB   Created test.
--    13 Mar 2015   RLB   Eliminate overlong lines.
--    29 Jan 2016   RLB   Corrected possible result of subtest2(4).
--
--!
with Report;
with TCTouch;
with Ada.Finalization;
procedure C760015 is

   procedure Raise_It (Num : Natural) is
   begin
      if Report.Equal (Num, Num) then
         raise Constraint_Error;
      end if;
   end Raise_It;


   procedure Subtest1 (Num : Natural) is
      -- Model a last-chance handler:
      type Last_Chance_Type is new Ada.Finalization.Limited_Controlled
         with null record;

      overriding
      procedure Initialize (Object : in out Last_Chance_Type);

      overriding
      procedure Finalize (Object : in out Last_Chance_Type);

      Data_with_Resources : Natural := Report.Ident_Int(0);
         -- This data models some data for which some resources need to
         -- be recovered when this subprogram exits.

      procedure Finalize (Object : in out Last_Chance_Type) is
      begin
         -- Free resources associated with Data_with_Resources.
         if Report.Equal (Data_with_Resources, 0) then
            TCTouch.Touch( 'f' ); ----------------------------------------- f
         else
            TCTouch.Touch( 'F' ); ----------------------------------------- F
         end if;
      end Finalize;

      procedure Initialize (Object : in out Last_Chance_Type) is
      begin
         -- Initialize last change handler:
         if Report.Equal (Data_with_Resources, 0) then
            TCTouch.Touch( 'I' ); ----------------------------------------- I
         else
            TCTouch.Touch( 'X' ); ----------------------------------------- X
         end if;
      end Initialize;

      Last_Chance : Last_Chance_Type;

   begin
      if Report.Equal (Num, 1) then
         TCTouch.Touch( 'S' ); ----------------------------------------- S
      elsif Report.Equal (Num, 2) then
         Data_with_Resources := Report.Ident_Int(12);
         TCTouch.Touch( 'E' ); ----------------------------------------- E
         Raise_It (Num); -- Raises Constraint_Error.
      elsif Report.Equal (Num, 3) then
         Data_with_Resources := Report.Ident_Int(52);
         TCTouch.Touch( 'R' ); ----------------------------------------- R
         return;
      else -- Not a subtest.
         raise Program_Error;
      end if;
   end Subtest1;


   procedure Subtest2 (Num : Natural) is
      -- Model a local container:
      type Map_Type is new Ada.Finalization.Controlled
         with record
            C : Character := '1';
          end record;

      overriding
      procedure Initialize (Object : in out Map_Type);

      overriding
      procedure Adjust (Object : in out Map_Type);

      overriding
      procedure Finalize (Object : in out Map_Type);

      Count : Natural := Report.Ident_Int(0);
         -- This data models some package-level data within the container
         -- package body (it would be instance data).

      procedure Finalize (Object : in out Map_Type) is
      begin
         -- Free resources associated with Data_with_Resources.
         if Report.Equal (Count, 1) then
            TCTouch.Touch( 'f' ); ----------------------------------------- f
         else
            TCTouch.Touch( 'F' ); ----------------------------------------- F
         end if;
      end Finalize;

      procedure Adjust (Object : in out Map_Type) is
      begin
         -- Free resources associated with Data_with_Resources.
         if Report.Equal (Count, 1) then
            TCTouch.Touch( 'a' ); ----------------------------------------- a
         else
            TCTouch.Touch( 'A' ); ----------------------------------------- A
         end if;
      end Adjust;

      procedure Initialize (Object : in out Map_Type) is
      begin
         -- Initialize last change handler:
         if Report.Equal (Count, 0) then
            TCTouch.Touch( 'I' ); ----------------------------------------- I
         else
            TCTouch.Touch( 'X' ); ----------------------------------------- X
         end if;
         Count := Count + 1;
      end Initialize;

      function Constructor (N : Natural) return Map_Type is
      begin
         if N = 1 then
             return (Ada.Finalization.Controlled with C => '3');
         else
             return Result : Map_Type;
         end if;
      end Constructor;

      procedure Sink (M : Map_Type) is
         -- To prevent removing M as a dead object.
      begin
         if Report.Equal (Character'Pos(M.C), 0) then
            Report.Failed ("Do not optimize M");
         end if;
      end Sink;

   begin
      if Report.Equal (Num, 1) then
         declare
            My_Map : Map_Type;
         begin
            TCTouch.Touch( 'S' ); ----------------------------------------- S
            Sink (My_Map);
         end;
      elsif Report.Equal (Num, 2) then
         declare
            My_Map : Map_Type := (Ada.Finalization.Controlled with C => '2');
         begin
            TCTouch.Touch( 'N' ); ----------------------------------------- N
            Sink (My_Map);
         end;
      elsif Report.Equal (Num, 3) then
         declare
            My_Map : Map_Type := Constructor (1);
         begin
            TCTouch.Touch( 'C' ); ----------------------------------------- C
            Sink (My_Map);
         end;
      elsif Report.Equal (Num, 4) then
         declare
            My_Map : Map_Type := Constructor (Num);
         begin
            TCTouch.Touch( 'c' ); ----------------------------------------- c
            Sink (My_Map);
         end;
      elsif Report.Equal (Num, 5) then
         declare
            My_Map : Map_Type;
         begin
            TCTouch.Touch( 'E' ); ----------------------------------------- E
            Sink (My_Map);
            Raise_It (Num); -- Raises Constraint_Error.
         end;
      elsif Report.Equal (Num, 6) then
         declare
            Map1, Map2 : Map_Type;
         begin
            TCTouch.Touch( 'R' ); ----------------------------------------- R
            Sink (Map1);
            Sink (Map2);
            return;
         end;
      else -- Not a subtest.
         raise Program_Error;
      end if;
   end Subtest2;


begin  -- Main test procedure.

   Report.Test ("C760015", "Check that a controlled type can be declared " &
                           "within a subprogram, and that an overridden " &
                           "Initialize or Finalize is called appropriately " &
                           "and can access local variables to the subprogram");

   Subtest1 (1);

   TCTouch.Validate (Expected => "ISf", Message => "Subtest1, 1");

   begin
      Subtest1 (2);
      TCTouch.Touch( 'Z' ); ----------------------------------------- Z
   exception
      when Constraint_Error =>
          TCTouch.Touch( 'H' ); ------------------------------------- H
   end;

   TCTouch.Validate (Expected => "IEFH", Message => "Subtest1, 2");

   Subtest1 (3);

   TCTouch.Validate (Expected => "IRF", Message => "Subtest1, 3");

   Subtest2 (1);

   TCTouch.Validate (Expected => "ISf", Message => "Subtest2, 1");

   Subtest2 (2);

   TCTouch.Validate (Expected => "NF", Message => "Subtest2, 2");

   Subtest2 (3);

   TCTouch.Validate_One_Of (Expected_1 => "AFAFCF", -- With temporary object
                                                    -- for both aggregate and
                                                    -- function result
                            Expected_2 => "AFCF",   -- With single temporary
                                                    -- object
                            Expected_3 => "CF",     -- Build-in-place
                            Message => "Subtest2, 3");

   Subtest2 (4);

   TCTouch.Validate_One_Of (Expected_1 => "Iafcf",  -- With temporary return
                                                    -- object
                            Expected_2 => "Icf",    -- Build-in-place
                            Message => "Subtest2, 4");

   begin
      Subtest2 (5);
      TCTouch.Touch( 'Z' ); ----------------------------------------- Z
   exception
      when Constraint_Error =>
          TCTouch.Touch( 'H' ); ------------------------------------- H
   end;

   TCTouch.Validate (Expected => "IEfH", Message => "Subtest2, 5");

   Subtest2 (6);

   TCTouch.Validate (Expected => "IXRFF", Message => "Subtest2, 6");


   Report.Result;

end C760015;
