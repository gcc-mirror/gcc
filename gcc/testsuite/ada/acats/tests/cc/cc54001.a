-- CC54001.A
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
--      Check that a general access-to-constant type may be passed as an
--      actual to a generic formal access-to-constant type.
--
-- TEST DESCRIPTION:
--      The generic implements a stack of access objects as an array. The
--      designated type of the formal access type is itself a formal private
--      type declared in the same generic formal part.
--      
--      The generic is instantiated with an unconstrained subtype of String,
--      which results in a stack which can accommodate strings of varying
--      lengths (ragged array). Furthermore, the access objects to be pushed
--      onto the stack are created both statically and dynamically, utilizing
--      allocators and the 'Access attribute.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Apr 96   SAIC    ACVC 2.1: Added pragma Elaborate to context clause
--                          preceding CC54001_1.
--
--!

generic
   Size : in Positive;
   type Element_Type (<>) is private;
   type Element_Ptr       is access constant Element_Type;
package CC54001_0 is -- Generic stack of pointers.

   type Stack_Type is private;

   procedure Push (Stack    : in out Stack_Type;
                   Elem_Ptr : in     Element_Ptr);

   procedure Pop  (Stack    : in out Stack_Type;
                   Elem_Ptr :    out Element_Ptr);

   -- ... Other operations.

private

   subtype Index is Positive range 1 .. (Size + 1);
   type Stack_Type is array (Index) of Element_Ptr;  -- Last element unused.

   Top : Index := 1;

end CC54001_0;


     --===================================================================--


package body CC54001_0 is

   procedure Push (Stack    : in out Stack_Type;
                   Elem_Ptr : in     Element_Ptr) is
   begin
      Stack(Top) := Elem_Ptr;
      Top := Top + 1;  -- Artificial: no Constraint_Error protection.
   end Push;


   procedure Pop (Stack    : in out Stack_Type;
                  Elem_Ptr :    out Element_Ptr) is
   begin
      Top := Top - 1;  -- Artificial: no Constraint_Error protection.
      Elem_Ptr := Stack(Top);
   end Pop;

end CC54001_0;


     --===================================================================--


with CC54001_0;      -- Generic stack of pointers.
pragma Elaborate (CC54001_0);

package CC54001_1 is

   subtype Message     is String;
   type    Message_Ptr is access constant Message;

   Message_Count : constant := 4;

   Message_0 : aliased constant Message := "Hello";
   Message_1 : aliased constant Message := "Doctor";
   Message_2 : aliased constant Message := "Name";
   Message_3 : aliased constant Message := "Continue";


   package Stack_of_Messages is new CC54001_0
     (Element_Type => Message,
      Element_Ptr  => Message_Ptr,
      Size         => Message_Count);

   Message_Stack : Stack_Of_Messages.Stack_Type;


   procedure Create_Message_Stack;

end CC54001_1;


     --===================================================================--


package body CC54001_1 is

   procedure Create_Message_Stack is
      -- Push access objects onto stack. Note that some are statically
      -- allocated, and some are dynamically allocated (using an aliased
      -- object to initialize).
   begin
      Stack_Of_Messages.Push (Message_Stack, Message_0'Access); -- Static.
      Stack_Of_Messages.Push (Message_Stack,
                              new Message'(Message_1));         -- Dynamic.
      Stack_Of_Messages.Push (Message_Stack, Message_2'Access); -- Static.
      Stack_Of_Messages.Push (Message_Stack,                    -- Dynamic.
                              new Message'(Message_3));
   end Create_Message_Stack;

end CC54001_1;


     --===================================================================--


with CC54001_1;

with Report;
procedure CC54001 is

   package Messages renames CC54001_1.Stack_Of_Messages;

   Msg0, Msg1, Msg2, Msg3 : CC54001_1.Message_Ptr;

begin
   Report.Test ("CC54001", "Check that a general access-to-constant type "   &
                           "may be passed as an actual to a generic formal " &
                           "access-to-constant type");

   CC54001_1.Create_Message_Stack;

   Messages.Pop (CC54001_1.Message_Stack, Msg3);  -- Pop items off stack in the
   Messages.Pop (CC54001_1.Message_Stack, Msg2);  -- reverse order that they
   Messages.Pop (CC54001_1.Message_Stack, Msg1);  -- were pushed.
   Messages.Pop (CC54001_1.Message_Stack, Msg0);

   if Msg0.all /= CC54001_1.Message_0 or else
      Msg1.all /= CC54001_1.Message_1 or else
      Msg2.all /= CC54001_1.Message_2 or else
      Msg3.all /= CC54001_1.Message_3
   then
      Report.Failed ("Items popped off of stack do not match those pushed");
   end if;

   Report.Result;
end CC54001;
