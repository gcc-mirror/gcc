-- FC70B00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation defines a generic list abstraction. List elements can
--      be of any (nonlimited) type. Lists are implemented as singly linked
--      lists. Access to list elements is sequential. For each list, pointers
--      are maintained to the first and last elements in the list, as well as
--      the next element to be accessed.
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

generic                           -- List abstraction.
   type Element_Type is private;  -- List elems can be of any nonlimited type.
package FC70B00 is

   type List_Type is limited private;

   -- Return true if current element is last in the list.
   function End_Of_List (L : List_Type) return Boolean;

   -- Read current element value; do NOT advance "current" pointer.
   procedure View_Element (L : in List_Type; E : out Element_Type);

   -- Read from current element and advance "current" pointer.
   procedure Read_Element (L : in out List_Type; E : out Element_Type);

   -- Write to current element and advance "current" pointer.
   procedure Write_Element (L : in out List_Type; E : in Element_Type);

   -- Add element to end of list.
   procedure Add_Element (L : in out List_Type; E : in Element_Type);

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

end FC70B00;


     --==================================================================--


package body FC70B00 is

   function End_Of_List (L : List_Type) return Boolean is
   begin
      return (L.Current = null);
   end End_Of_List;


   procedure View_Element (L : in List_Type; E : out Element_Type) is
   begin
      -- ... Error-checking code omitted for brevity.
      E := L.Current.Item;               -- Retrieve current element.
   end View_Element;


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


   procedure Reset (L : in out List_Type) is
   begin
      L.Current := L.First;              -- Set "current" pointer to first
   end Reset;                            -- list element.


end FC70B00;
