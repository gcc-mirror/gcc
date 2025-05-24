-- CC54002.A
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
--      Check that a general access-to-variable type may be passed as an
--      actual to a generic formal general access-to-variable type. Check that
--      designated objects may be read and updated through the access value.
--
-- TEST DESCRIPTION:
--      The generic implements a List of access objects as an array, which
--      is itself a component of a record. The designated type of the formal
--      access type is a formal private type declared in the same generic
--      formal part.
--
--      The access objects to be placed in the List are created both
--      statically and dynamically, utilizing allocators and the 'Access
--      attribute.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Apr 96   SAIC    ACVC 2.1: Added pragma Elaborate to context clause
--                          preceding CC54002_1.
--
--!

generic
   Size : in Positive;
   type Element_Type (<>) is private;
   type Element_Ptr       is access all Element_Type;
package CC54002_0 is -- Generic list of pointers.

   subtype Index is Positive range 1 .. (Size + 1);

   type List_Array is array (Index) of Element_Ptr;

   type List_Type is record
      Elements : List_Array;
      Next     : Index := 1;    -- Next available "slot" in list.
   end record;


   procedure Put (List     : in out List_Type;
                  Elem_Ptr : in     Element_Ptr;
                  Location : in     Index);

   procedure Get (List     : in out List_Type;
                  Elem_Ptr :    out Element_Ptr;
                  Location : in     Index);

   -- ... Other operations.

end CC54002_0;


     --===================================================================--


package body CC54002_0 is

   procedure Put (List     : in out List_Type;
                  Elem_Ptr : in     Element_Ptr;
                  Location : in     Index) is
   begin
      List.Elements(Location) := Elem_Ptr;
   end Put;


   procedure Get (List     : in out List_Type;
                  Elem_Ptr :    out Element_Ptr;
                  Location : in     Index) is
   begin  -- Artificial: no provision for getting "empty" element.
      Elem_Ptr := List.Elements(Location);
   end Get;

end CC54002_0;


     --===================================================================--


with CC54002_0;      -- Generic List of pointers.
pragma Elaborate (CC54002_0);

package CC54002_1 is

   subtype Lengths is Natural range 0 .. 50;

   type Subscriber (NLen, ALen: Lengths := 50) is record
      Name    : String(1 .. NLen);
      Address : String(1 .. ALen);
      -- ... Other components.
   end record;

   type Subscriber_Ptr is access all Subscriber;         -- General access-to-
                                                         -- variable type.

   package District_Subscription_Lists is new CC54002_0
     (Element_Type => Subscriber,
      Element_Ptr  => Subscriber_Ptr,
      Size         => 100);

   District_01_Subscribers : District_Subscription_Lists.List_Type;


   New_Subscriber_01 : aliased CC54002_1.Subscriber :=
     (12, 23, "Brown, Silas", "King's Pyland, Dartmoor");

   New_Subscriber_02 : aliased CC54002_1.Subscriber :=
     (16, 23, "Hatherly, Victor", "16A Victoria St. London");

end CC54002_1;

-- No body for CC54002_1.


     --===================================================================--


with CC54002_1;

with Report;
procedure CC54002 is

   Mod_Subscriber_01 : constant CC54002_1.Subscriber :=
     (12, 23, "Brown, Silas", "Mapleton, Dartmoor     ");

   TC_Actual_01, TC_Actual_02 : CC54002_1.Subscriber_Ptr;


   use type CC54002_1.Subscriber;  -- "/=" directly visible.

begin
   Report.Test ("CC54002", "Check that a general access-to-variable type "   &
                           "may be passed as an actual to a generic formal " &
                           "access-to-variable type");


   -- Add elements to the list:

   CC54002_1.District_Subscription_Lists.Put    -- Element created statically.
     (List     => CC54002_1.District_01_Subscribers,
      Elem_Ptr => CC54002_1.New_Subscriber_01'Access,
      Location => 1);

   CC54002_1.District_Subscription_Lists.Put    -- Element created dynamically.
     (List     => CC54002_1.District_01_Subscribers,
      Elem_Ptr => new CC54002_1.Subscriber'(CC54002_1.New_Subscriber_02),
      Location => 2);


   -- Manipulation of the objects on the list is performed below directly
   -- through the access objects. Although such manipulation is artificial
   -- from the perspective of this usage model, it is not artificial in
   -- general and is necessary in order to test the objective.


   -- Modify the first list element through the access object:

   CC54002_1.District_01_Subscribers.Elements(1).Address :=      -- Update
      "Mapleton, Dartmoor     ";      -- Implicit dereference.   -- through the
                                                                 -- access
                                                                 -- object.
   -- Retrieve elements of the list:

   CC54002_1.District_Subscription_Lists.Get
     (CC54002_1.District_01_Subscribers,
      TC_Actual_01,
      1);

   CC54002_1.District_Subscription_Lists.Get
     (CC54002_1.District_01_Subscribers,
      TC_Actual_02,
      2);

   -- Verify list contents in two ways: 1st verify the directly-dereferenced
   -- access objects against the dereferenced access objects returned by Get;
   -- 2nd verify them against objects the expected values:

                                                                 -- Read
                                                                 -- through the
                                                                 -- access
                                                                 -- objects.

   if CC54002_1.District_01_Subscribers.Elements(1).all /= TC_Actual_01.all
      or else
      CC54002_1.District_01_Subscribers.Elements(2).all /= TC_Actual_02.all
   then
      Report.Failed ("Wrong results returned by Get");
   
   elsif CC54002_1.District_01_Subscribers.Elements(1).all /=
           Mod_Subscriber_01
         or
         CC54002_1.District_01_Subscribers.Elements(2).all /=
           CC54002_1.New_Subscriber_02
   then
      Report.Failed ("List elements do not have expected values");
   end if;

   Report.Result;
end CC54002;
