-- CC70B01.A
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
--      Check that a formal package actual part may specify actual parameters
--      for a generic formal package. Check that a use clause in the generic
--      formal part provides direct visibility of declarations within the
--      generic formal package. Check that the scope of such a use clause
--      extends to the generic subprogram body. Check that the visible part of
--      the generic formal package includes the first list of basic
--      declarative items of the package specification.
--
--      Check the case where the formal package is declared in a generic
--      subprogram.
--
-- TEST DESCRIPTION:
--      Declare a list abstraction in a generic package which manages lists of
--      elements of any nonlimited type (foundation code). Declare a generic
--      subprogram which operates on lists of elements of discrete types.
--      Provide the generic subprogram with two formal parameters: (1) a
--      formal discrete type which represents a list element type, and (2) a
--      generic formal package with the list abstraction package as template.
--      Use the formal discrete type as the generic formal actual part for the
--      formal package. Include a use clause for the formal package in the
--      generic subprogram formal part.
--
-- TEST FILES:
--      The following files comprise this test:
--
--         FC70B00.A
--         CC70B01.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

-- Declare a generic subprogram which performs an operation on lists of
-- discrete objects.

with FC70B00;  -- Generic list abstraction.
generic

   -- Import the list abstraction defined in FC70B00. To ensure that only
   -- list abstraction instances defining lists of *discrete* elements will be
   -- accepted as actuals to this generic, declare a formal discrete type and
   -- pass it as an actual parameter to the formal package.
   --
   -- Only instances declared for the same discrete type as that used to
   -- instantiate this generic subprogram will be accepted.

   type Elem_Type is (<>);                  -- Discrete types only.
   with package List_Mgr is new FC70B00 (Elem_Type);

   use List_Mgr;                            -- Use clause for formal package.

procedure CC70B01_0 (L : in out List_Type); -- List_Mgr.List_Type directly
                                            -- visible.


     --==================================================================--


procedure CC70B01_0 (L : in out List_Type) is  -- Declarations in List_Mgr
begin                                          -- still directly visible.
   Reset (L);
   while not End_Of_List (L) loop
      Write_Element (L, Elem_Type'First);      -- This statement assumes
   end loop;                                   -- Elem_Type is discrete.
end CC70B01_0;


     --==================================================================--


with FC70B00;    -- Generic list abstraction.
with CC70B01_0;  -- Generic "zeroing" operation for lists of discrete types.

with Report;
procedure CC70B01 is

   type Points is range 0 .. 10;                    -- Discrete type.
   package Lists_of_Scores is new FC70B00 (Points); -- List-of-points
                                                    -- abstraction.
   Scores : Lists_of_Scores.List_Type;              -- List of points.

   procedure Reset_All_Scores is new                -- Operation on lists of
     CC70B01_0 (Points, Lists_of_Scores);           -- points.


   -- Begin test code declarations: -----------------------

   type TC_Score_Array is array (1 .. 3) of Points;

   TC_Initial_Values : constant TC_Score_Array := (2, 4, 6);
   TC_Final_Values   : constant TC_Score_Array := (0, 0, 0);

   TC_Correct_Initial_Values : Boolean := False;
   TC_Correct_Final_Values   : Boolean := False;


   procedure TC_Initialize_List (L : in out Lists_of_Scores.List_Type) is
   begin                                  -- Initial list contains 3 scores
      for I in TC_Score_Array'Range loop  -- with the values 2, 4, and 6.
         Lists_of_Scores.Add_Element (L, TC_Initial_Values(I));
      end loop;
   end TC_Initialize_List;


   procedure TC_Verify_List (L        : in out Lists_of_Scores.List_Type;
                             Expected : in     TC_Score_Array;
                             OK       :    out Boolean) is
      Actual : TC_Score_Array;
   begin                                  -- Verify that all scores have been
      Lists_of_Scores.Reset (L);          -- set to zero.
      for I in TC_Score_Array'Range loop
         Lists_of_Scores.Read_Element (L, Actual(I));
      end loop;
      OK := (Actual = Expected);
   end TC_Verify_List;

   -- End test code declarations. -------------------------


begin
   Report.Test ("CC70B01", "Check that a library-level generic subprogram "   &
                "may have a formal package as a formal parameter, and that "  &
                "the generic formal actual part may specify explicit actual " &
                "parameters. Check that a use clause is legal in the "        &
                "generic formal part");

   TC_Initialize_List (Scores);
   TC_Verify_List (Scores, TC_Initial_Values, TC_Correct_Initial_Values);

   if not TC_Correct_Initial_Values then
      Report.Failed ("List contains incorrect initial values");
   end if;

   Reset_All_Scores (Scores);
   TC_Verify_List (Scores, TC_Final_Values, TC_Correct_Final_Values);

   if not TC_Correct_Final_Values then
      Report.Failed ("List contains incorrect final values");
   end if;

   Report.Result;
end CC70B01;
