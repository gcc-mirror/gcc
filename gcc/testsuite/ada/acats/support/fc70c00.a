-- FC70C00.A
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
--      This foundation defines a generic list abstraction in two packages.
--      The first package declares the types, the second declares the
--      operations. List elements can be of any (nonlimited) type. Lists are
--      implemented as singly linked lists. Access to list elements is
--      sequential. For each list, pointers are maintained to the first and
--      last elements in the list, as well as the next element to be accessed.
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

generic             
   type Element_Type is private;  -- List elems may be of any nonlimited type.
package FC70C00_0 is              -- List abstraction.

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

end FC70C00_0;


     --==================================================================--


-- No body for FC70C00_0;


     --==================================================================--


with FC70C00_0;                -- List abstraction.
generic
   with package List_Mgr is new FC70C00_0 (<>);
package FC70C00_1 is           -- Basic list operations.

   -- Return true if current element is last in the list.
   function End_Of_List (L : List_Mgr.List_Type) return Boolean;

   -- Set "current" pointer to first list element.
   procedure Reset (L : in out List_Mgr.List_Type);

end FC70C00_1;


     --==================================================================--


package body FC70C00_1 is

   function End_Of_List (L : List_Mgr.List_Type) return Boolean is
      use List_Mgr;  -- Renders "=" directly visible.
   begin
      return (L.Current = null);
   end End_Of_List;


   procedure Reset (L : in out List_Mgr.List_Type) is
   begin
      L.Current := L.First;                 -- Set "current" pointer to first
   end Reset;                               -- list element.

end FC70C00_1;
