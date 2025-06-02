--  CDB3A01.A
--
--                            Grant of Unlimited Rights
--
--    The Ada Conformity Assessment Authority (ACAA) holds unlimited
--    rights in the software and documentation contained herein. Unlimited
--    rights are the same as those granted by the U.S. Government for older
--    parts of the Ada Conformity Assessment Test Suite, and are defined
--    in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--    intends to confer upon all recipients unlimited rights equal to those
--    held by the ACAA. These rights include rights to use, duplicate,
--    release or disclose the released technical data and computer software
--    in whole or in part, in any manner and for any purpose whatsoever, and
--    to have or permit others to do so.
--
--                                    DISCLAIMER
--
--    ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--    DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--    WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--    SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--    OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--*
--  OBJECTIVE:
--    Check that the Default_Storage_Pool aspect may be specified for a
--    generic instance. The aspect must override any Default_Storage_Pool
--    pragma that might apply to the generic unit. If the aspect is not
--    specified, the default pool of the instance is that defined for the
--    generic unit.
--
--  CHANGE HISTORY:
--     09 Oct 2014 BM  Initial Version.
--     20 Nov 2014 RLB Cleaned up and renamed for release.
--     24 Nov 2014 RLB Corrected so pool water-lines adjust for implementation
--                     characteristics.
--     12 Mar 2015 RLB Fixed overlength lines.
--!

with FDB3A00;
with Report; use Report;
with TCTouch;
with System.Storage_Elements;
with Ada.Exceptions;

procedure CDB3A01 is
   use type System.Storage_Elements.Storage_Count;
   subtype Message is String(1..10);
   Max_Message_Size : constant System.Storage_Elements.Storage_Count :=
        (Message'Size / System.Storage_Elements.Storage_Element'Size) + 1;
   Water_Line_Size : constant System.Storage_Elements.Storage_Count :=
      Max_Message_Size + (Max_Message_Size / 2); -- Room for 1.5 messages.

   Pool1 : FDB3A00.Stack_Heap (Water_Line => Water_Line_Size, TC_Id => '1');
   Pool2 : FDB3A00.Stack_Heap (Water_Line => Water_Line_Size, TC_Id => '2');

   pragma Default_Storage_Pool (Pool1);

   generic
      type X is private;
   package P1 is
      type Node is private;

      function Create (Item : X; Description : Message) return Node;

   private
      type Message_Access is access Message;

      type Node is record
         Item : X;
         Val  : Message_Access;
      end record;

   end P1;

   package body P1 is
      function Create (Item : X; Description : Message) return Node is
         Result : Node;
      begin
         Result := (Item => Item, Val => new Message'(Description));
         return Result;
      end Create;
   end P1;

   Tiny_Pool : FDB3A00.Stack_Heap (Water_Line => 1, TC_Id => 'T');

   pragma Default_Storage_Pool (Tiny_Pool);

   package My_P1 is new P1 (X => Float);

   package Pool_Override_P1 is new P1 (X => Character)
      with Default_Storage_Pool => Pool2;

   Data1 : My_P1.Node;
   Data2 : Pool_Override_P1.Node;

begin

   Test
     (Name  => "CDB3A01",
      Descr => "Testing the Default_Storage_Pool aspect with generic " &
               "instantiations");

   if (FDB3A00.TC_Largest_Request /= 0) then
      Failed
        (Descr => "Unexpected Initial State: Pool has existing allocations");
   end if;

   Comment ("Allocating from default pool of generic unit");

   begin
      Data1 := My_P1.Create (Item => 3.14, Description => "1234567890");

      TCTouch.Validate
        (Expected => "A",
         Message  =>
           "Pool was not used for allocation, Largest=" &
           System.Storage_Elements.Storage_Count'Image
             (FDB3A00.TC_Largest_Request),
         Order_Meaningful => True);

      Comment
        ("Allocated" &
         System.Storage_Elements.Storage_Count'Image
           (FDB3A00.TC_Largest_Request) &
           " bytes");

   exception
      when Err:FDB3A00.Pool_Overflow =>
         Failed (Descr => "Exception raised. Allocating from wrong pool; " &
                          "should use pool 1");
         Comment (Descr => "Allocated from " &
                            Ada.Exceptions.Exception_Message(Err));

   end;

   Comment ("Allocating from alternate pool specified by instance aspect");

   begin

      Data2 :=
        Pool_Override_P1.Create (Item => '?', Description => "ABCDEFGHIJ");

      TCTouch.Validate
        (Expected => "A",
         Message  =>
           "Pool was not used for allocation, Largest=" &
           System.Storage_Elements.Storage_Count'Image
             (FDB3A00.TC_Largest_Request),
         Order_Meaningful => True);

   exception
      when Err:FDB3A00.Pool_Overflow =>
         Failed
           (Descr =>
              "Exception raised. Default Pool of generic unit" &
              " was not overridden (should use pool 2)");
         Comment (Descr => "Allocated from " &
                            Ada.Exceptions.Exception_Message(Err));
   end;

   Result;
end CDB3A01;
