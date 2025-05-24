-- CC70001.A
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
--      Check that the template for a generic formal package may be a child
--      package, and that a child instance which is an instance of the
--      template may be passed as an actual to the formal package. Check that
--      the visible part of the generic formal package includes the first list
--      of basic declarative items of the package specification.
--
-- TEST DESCRIPTION:
--      Declare a list abstraction in a generic package which manages lists of
--      elements of any nonlimited type. Declare a generic child package of
--      this package which defines additional list operations. Declare a
--      generic subprogram which operates on lists of elements of discrete
--      types. Provide the generic subprogram with three formal parameters:
--      (1) a formal discrete type which represents a list element type, (2)
--      a generic formal package with the parent list generic as template, and
--      (3) a generic formal package with the child list generic as template.
--      Use the formal discrete type as the generic formal actual part for the
--      parent formal package. In the main program, declare an instance of
--      parent, then declare an instance of the child which is itself a child
--      the parent's instance. Pass these instances as actuals to the generic
--      subprogram instance.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      05 Nov 95   SAIC    ACVC 2.0.1 fixes: Corrected syntax of formal
--                          package declaration.
--      27 Feb 97   PWB.CTA Added an elaboration pragma.
--!

generic             
   type Element_Type is private;  -- List elems may be of any nonlimited type.
package CC70001_0 is              -- List abstraction.

   type List_Type is limited private;


   -- Return true if current element is last in the list.
   function End_Of_List (L : List_Type) return Boolean;

   -- Set "current" pointer to first list element.
   procedure Reset (L : in out List_Type);

private

   type Node_Type;
   type Node_Pointer is access Node_Type;

   type Node_Type is record
      Item : Element_Type;
      Next : Node_Pointer;
   end record;

   type List_Type is record
      First   : Node_Pointer;
      Current : Node_Pointer;
      Last    : Node_Pointer;
   end record;

end CC70001_0;


     --==================================================================--


package body CC70001_0 is

   function End_Of_List (L : List_Type) return Boolean is
   begin
      return (L.Current = null);
   end End_Of_List;


   procedure Reset (L : in out List_Type) is
   begin
      L.Current := L.First;                 -- Set "current" pointer to first
   end Reset;                               -- list element.

end CC70001_0;


     --==================================================================--


-- Child must be generic since parent is generic. A formal parameter for
-- "element type" can not be provided here, because then the type of list
-- element assumed by these new operations would be different from that
-- defined by the list type declared in the parent.

generic             
package CC70001_0.CC70001_1 is    -- Additional list operations.

   -- Read from current element and advance "current" pointer.
   procedure Read_Element (L : in out List_Type; E : out Element_Type);

   -- Write to current element and advance "current" pointer.
   procedure Write_Element (L : in out List_Type; E : in Element_Type);

   -- Add element to end of list.
   procedure Add_Element (L : in out List_Type; E : in Element_Type);

end CC70001_0.CC70001_1;


     --==================================================================--


package body CC70001_0.CC70001_1 is

   procedure Read_Element (L : in out List_Type; E : out Element_Type) is
   begin
      -- ... Error-checking code omitted for brevity.
      E         := L.Current.Item;       -- Retrieve current element.
      L.Current := L.Current.Next;       -- Advance "current" pointer.
   end Read_Element;


   procedure Write_Element (L : in out List_Type; E : in Element_Type) is
   begin
      -- ... Error-checking code omitted for brevity.
      L.Current.Item := E;               -- Write to current element.
      L.Current      := L.Current.Next;  -- Advance "current" pointer.
   end Write_Element;


   procedure Add_Element (L : in out List_Type; E : in Element_Type) is
      New_Node : Node_Pointer := new Node_Type'(E, null); 
   begin
      if L.First = null then             -- No elements in list, so add new
         L.First := New_Node;            -- element at beginning of list.
      else
         L.Last.Next := New_Node;        -- Add new element at end of list.
      end if;
      L.Last := New_Node;                -- Set last-in-list pointer.
   end Add_Element;

end CC70001_0.CC70001_1;


     --==================================================================--


with CC70001_0.CC70001_1;  -- Generic list abstraction + additional operations.
generic

   -- Import the list abstraction defined in CC70001_0, as well as the
   -- additional operations defined in CC70001_0.CC70001_1. Declare a formal
   -- discrete type. Restrict this generic procedure to operate only on lists
   -- of discrete elements by passing the formal discrete type as an actual
   -- parameter to the formal (parent) package.

   type Elem_Type is (<>);                  -- Discrete types only.
   with package List_Mgr is new CC70001_0 (Elem_Type);
   with package List_Ops is new List_Mgr.CC70001_1 (<>);

procedure CC70001_2 (L : in out List_Mgr.List_Type);


     --==================================================================--


procedure CC70001_2 (L : in out List_Mgr.List_Type) is
begin
   List_Mgr.Reset (L);
   while not List_Mgr.End_Of_List (L) loop
      List_Ops.Write_Element (L, Elem_Type'First);
   end loop;
end CC70001_2;


     --==================================================================--


package CC70001_3 is

   type Points is range 0 .. 10;

   -- ... Various other types used by the application.

end CC70001_3;


-- No body for CC70001_3;


     --==================================================================--


-- Declare instances of the generic list packages for the discrete type.
-- In order to establish that the type passed as an actual to the parent
-- generic (CC70001_0) is the one utilized by the child generic (CC70001_1),
-- the instance of the child must itself be declared as a child of the
-- instance of the parent. Since only library units may have or be children,
-- both instances must be library units.

with CC70001_0;            -- Generic list abstraction.
with CC70001_3;            -- Package containing discrete type declaration.
pragma Elaborate (CC70001_0);
package CC70001_4 is new CC70001_0 (CC70001_3.Points);

with CC70001_0.CC70001_1;  -- Generic extension to list abstraction.
with CC70001_4;
package CC70001_4.CC70001_5 is new CC70001_4.CC70001_1;


     --==================================================================--


with CC70001_2;            -- Generic "zeroing" op for lists of discrete types.
with CC70001_3;            -- Types for application.
with CC70001_4.CC70001_5;  -- Discrete list abstraction + additional ops.

with Report;
procedure CC70001 is

   package Lists_Of_Scores renames CC70001_4;
   package Score_Ops       renames CC70001_4.CC70001_5;

   Scores : Lists_Of_Scores.List_Type;                -- List of points.

   procedure Reset_All_Scores is new CC70001_2        -- Operation on lists of
     (Elem_Type => CC70001_3.Points,                  -- points.
      List_Mgr  => Lists_Of_Scores,
      List_Ops  => Score_Ops);


   -- Begin test code declarations: -----------------------

   type TC_Score_Array is array (1 .. 3) of CC70001_3.Points;

   TC_Initial_Values : constant TC_Score_Array := (2, 4, 6);
   TC_Final_Values   : constant TC_Score_Array := (0, 0, 0);

   TC_Correct_Initial_Values : Boolean := False;
   TC_Correct_Final_Values   : Boolean := False;


   procedure TC_Initialize_List (L : in out Lists_of_Scores.List_Type) is
   begin                                  -- Initial list contains 3 scores
      for I in TC_Score_Array'Range loop  -- with the values 2, 4, and 6.
         Score_Ops.Add_Element (L, TC_Initial_Values(I));
      end loop;
   end TC_Initialize_List;


   procedure TC_Verify_List (L        : in out Lists_of_Scores.List_Type;
                             Expected : in     TC_Score_Array;
                             OK       :    out Boolean) is
      Actual : TC_Score_Array;
   begin                                  -- Verify that all scores have been
      Lists_of_Scores.Reset (L);          -- set to zero.
      for I in TC_Score_Array'Range loop
         Score_Ops.Read_Element (L, Actual(I));
      end loop;
      OK := (Actual = Expected);
   end TC_Verify_List;

   -- End test code declarations. -------------------------


begin
   Report.Test ("CC70001", "Check that the template for a generic formal "   &
                "package may be a child package, and that a child instance " &
                "which is an instance of the template may be passed as an "  &
                "actual to the formal package. Check that the visible part " &
                "of the generic formal package includes the first list of "  &
                "basic declarative items of the package specification");

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
end CC70001;
