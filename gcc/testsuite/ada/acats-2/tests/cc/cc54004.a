-- CC54004.A
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
--      Check that the designated type of a generic formal pool-specific
--      access type may be class-wide. Check that calls to primitive
--      subprograms in the instance dispatch to the appropriate bodies when
--      the controlling operand is a dereference of an object of the access-
--      to-class-wide type.
--
-- TEST DESCRIPTION:
--      A hierarchy of types is declared in two packages. The root type of
--      the class is declared as abstract in a separate package. It possesses
--      an abstract primitive subprogram Handle. A concrete type extends the
--      root type in a second package with a component of an enumeration type.
--      A second type extends this extension in the same package. Both
--      derivatives override the root type's primitive subprogram with a
--      non-abstract subprogram.
--
--      The generic implements a heterogeneous stack of access-to-class-wide
--      objects in the root type's class. A subprogram declared in the
--      generic calls Handle using dereferences of each of the class-wide
--      objects on the stack as operand. Each call to Handle should dispatch
--      to the appropriate body based on the tag of the operand. The
--      overriding versions of Handle each set the component of the type to
--      a different value. The value of the component is checked to verify
--      that the calls dispatched correctly.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Apr 96   SAIC    ACVC 2.1: Added pragma Elaborate to context clause
--                          preceding CC54004_3.
--
--!

package CC54004_0 is

   -- The types and operations defined here are artificial. The component
   -- TC_Code is the only component required for testing purposes.

   type TC_Code_Type is (None, Low, Medium);

   type Alert is abstract tagged record              -- Abstract type.
      TC_Code : TC_Code_Type;    -- Testing flag.
   end record;

   procedure Handle (A : in out Alert);              -- Non-abstract primitive
                                                     -- subprogram.
   -- ...Other operations.

   type Alert_Ptr is access Alert'Class;             -- Access-to-class-wide
                                                     -- type.
end CC54004_0;


     --===================================================================--


package body CC54004_0 is

   procedure Handle (A : in out Alert) is
   begin
      A.TC_Code := None;
   end Handle;

end CC54004_0;


     --===================================================================--


with CC54004_0;
use  CC54004_0;
package CC54004_1 is

   type Low_Alert is new CC54004_0.Alert with record
      C1 : String (1 .. 5) := "Dummy";
      -- ...Other components.
   end record;

   procedure Handle (A : in out Low_Alert);          -- Overrides parent's
                                                     -- operations.
   --...Other operations.


   type Medium_Alert is new Low_Alert with record
      C : Integer := 6;
      -- ...Other components.
   end record;

   procedure Handle (A : in out Medium_Alert);       -- Overrides parent's
                                                     -- operations.
   --...Other operations.

end CC54004_1;


     --===================================================================--

package body CC54004_1 is

   procedure Handle (A : in out Low_Alert) is
   begin
      A.TC_Code := Low;
   end Handle;

   procedure Handle (A : in out Medium_Alert) is
   begin
      A.TC_Code := Medium;
   end Handle;

end CC54004_1;


     --===================================================================--


with CC54004_0;
generic
   type Element_Type is abstract new CC54004_0.Alert with private;
   type Element_Ptr  is access Element_Type'Class;
package CC54004_2 is

   type Stack_Type is private;

   procedure Push (Stack     : in out Stack_Type;
                   Elem_Ptr  : in     Element_Ptr);

   procedure Pop  (Stack     : in out Stack_Type;
                   Elem_Ptr  :    out Element_Ptr);

   procedure Process_Stack (Stack : in out Stack_Type);

   -- ... Other operations.

private

   subtype Index is Positive range 1 .. 5;
   type Stack_Type is array (Index) of Element_Ptr;

   Top : Index := 1;

end CC54004_2;


     --===================================================================--


package body CC54004_2 is

   procedure Push (Stack    : in out Stack_Type;
                   Elem_Ptr : in     Element_Ptr) is
   begin
      Stack(Top) := Elem_Ptr;
      Top := Top + 1;     -- Artificial: no Constraint_Error protection.
   end Push;


   procedure Pop  (Stack     : in out Stack_Type;
                   Elem_Ptr  :    out Element_Ptr)is
   begin
      Top := Top - 1;     -- Artificial: no Constraint_Error protection.
      Elem_Ptr := Stack(Top);
   end Pop;


   -- Call Handle for each element on the stack. Since the dereferenced access
   -- object is of a class-wide type, all calls to Handle are dispatching. The
   -- version of Handle called will be that declared for the type
   -- corresponding to the tag of the operand.

   procedure Process_Stack (Stack  : in out Stack_Type) is
   begin -- Artificial: no Constraint_Error protection.
      for I in reverse Index'First .. (Top - 1) loop
         Handle (Stack(I).all);            -- Call dispatches based on
      end loop;                            -- tag of operand.
   end Process_Stack;

end CC54004_2;


     --===================================================================--


with CC54004_0;
with CC54004_1;
with CC54004_2;
pragma Elaborate (CC54004_2);

package CC54004_3 is

   package Alert_Stacks is new CC54004_2 (Element_Type => CC54004_0.Alert,
                                          Element_Ptr  => CC54004_0.Alert_Ptr);

   -- All overriding versions of Handle visible at the point of instantiation.

   Alert_List  : Alert_Stacks.Stack_Type;

   procedure TC_Create_Alert_Stack;

end CC54004_3;


     --===================================================================--


package body CC54004_3 is

   procedure TC_Create_Alert_Stack is
   begin
      Alert_Stacks.Push (Alert_List, new CC54004_1.Low_Alert);
      Alert_Stacks.Push (Alert_List, new CC54004_1.Medium_Alert);
   end TC_Create_Alert_Stack;

end CC54004_3;


     --===================================================================--


with CC54004_0;
with CC54004_1;
with CC54004_3;

with Report;
procedure CC54004 is
   TC_Low_Ptr, TC_Med_Ptr : CC54004_0.Alert_Ptr;
   TC_Low_Actual          : CC54004_1.Low_Alert;
   TC_Med_Actual          : CC54004_1.Medium_Alert;

   use type CC54004_0.TC_Code_Type;
begin
   Report.Test ("CC54004", "Check that the designated type of a generic " &
                "formal pool-specific access type may be class-wide");


   -- Create stack of elements:

   CC54004_3.TC_Create_Alert_Stack;


   -- Commence dispatching operations on stack elements:

   CC54004_3.Alert_Stacks.Process_Stack (CC54004_3.Alert_List);


   -- Pop "handled" alerts off stack:

   CC54004_3.Alert_Stacks.Pop (CC54004_3.Alert_List, TC_Med_Ptr);
   CC54004_3.Alert_Stacks.Pop (CC54004_3.Alert_List, TC_Low_Ptr);


   -- Verify results:

   if TC_Low_Ptr.all not in CC54004_1.Low_Alert    or else
      TC_Med_Ptr.all not in CC54004_1.Medium_Alert
   then
      Report.Failed ("Class-wide objects do not have expected tags");

   -- The explicit dereference of the "Pop"ed pointers results in views of
   -- the designated objects, the nominal subtypes of which are class-wide.
   -- In order to be able to reference the component TC_Code, these views
   -- must be converted to a specific type possessing that component.

   elsif CC54004_1.Low_Alert(TC_Low_Ptr.all).TC_Code    /= CC54004_0.Low    or
         CC54004_1.Medium_Alert(TC_Med_Ptr.all).TC_Code /= CC54004_0.Medium
   then
      Report.Failed ("Calls did not dispatch to expected operations");
   end if;

   Report.Result;
end CC54004;
