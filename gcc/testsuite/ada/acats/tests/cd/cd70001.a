--
-- CD70001.A
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
--      Check that package System includes Max_Base_Digits, Address,
--      Null_Address, Word_Size, functions "<", "<=", ">", ">=", "="
--      (with Address parameters and Boolean results), Bit_Order,
--      Default_Bit_Order, Any_Priority, Interrupt_Priority,
--      and Default_Priority.
--
--      Check that package System.Storage_Elements includes all required
--      types and operations.
--
-- TEST DESCRIPTION:
--      The test checks for the existence of the names additional
--      to package system above those names tested for in 9Xbasic.
--
--      This test checks that the semantics provided in Storage_Elements
--      are present and operate marginally within expectations (to the best
--      extent possible in a portable implementation independent fashion).
--
--
-- CHANGE HISTORY:
--      09 MAY 95   SAIC   Initial version
--      27 JAN 96   SAIC   Revised for 2.1; Allow negative address delta
--
--!

with Report;
with Ada.Text_IO;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
procedure CD70001 is
  use System;

  procedure CD70 is

    type Int_Max is range Min_Int .. Max_Int;

    My_Int : Int_Max := System.Max_Base_Digits + System.Word_Size;

    An_Address : Address;
    An_Other_Address : Address := An_Address'Address;

  begin  -- 7.0


      if Default_Bit_Order not in High_Order_First..Low_Order_First then
         Report.Failed ("Default_Bit_Order invalid");
      end if;

      if Bit_Order'Pos(High_Order_First) /= 0 then
         Report.Failed ("Bit_Order'Pos(High_Order_First) /= 0");
      end if;

      if Bit_Order'Pos(Low_Order_First) /= 1 then
         Report.Failed ("Bit_Order'Pos(Low_Order_First) /= 1");
      end if;

      An_Address := My_Int'Address;

      if An_Address = Null_Address then
        Report.Failed ("Null_Address matched a real address");
      end if;


      if An_Address'Address /= An_Other_Address then
        Report.Failed("Value set at elaboration not equal to itself");
      end if;

      if An_Address'Address > An_Other_Address 
         and An_Address'Address < An_Other_Address then
        Report.Failed("Address is both greater and less!");
      end if;

      if not (An_Address'Address >= An_Other_Address 
         and An_Address'Address <= An_Other_Address) then
        Report.Failed("Address comparisons wrong");
      end if;


      if Priority'First /= Any_Priority'First then
        Report.Failed ("Priority'First /= Any_Priority'First");
      end if;

      if Interrupt_Priority'First /= Priority'Last+1 then
        Report.Failed ("Interrupt_Priority'First /= Priority'Last+1");
      end if;

      if Interrupt_Priority'Last /= Any_Priority'Last then
        Report.Failed ("Interrupt_Priority'Last /= Any_Priority'Last");
      end if;

      if Default_Priority /= ((Priority'First + Priority'Last)/2) then
        Report.Failed ("Default_Priority wrong value");
      end if;

  end CD70;

  procedure CD71 is
    use System.Storage_Elements;

    Storehouse_1 : Storage_Array(0..127);
    Storehouse_2 : Storage_Array(0..127);

    House_Offset : Storage_Offset;

  begin  -- 7.1


      if Storage_Count'First /= 0 then
        Report.Failed ("Storage_Count'First /= 0");
      end if;

      if Storage_Count'Last /= Storage_Offset'Last then
        Report.Failed ("Storage_Count'Last /= Storage_Offset'Last");
      end if;


      if Storage_Element'Size /= Storage_Unit then
        Report.Failed ("Storage_Element'Size /= Storage_Unit");
      end if;

      if Storage_Array'Component_Size /= Storage_Unit then
        Report.Failed ("Storage_Array'Element_Size /= Storage_Unit");
      end if;

      if Storage_Element'Last+1 /= 0 then
        Report.Failed ("Storage_Element not modular");
      end if;


      -- "+", "-"( Address, Storage_Offset) and inverse

      House_Offset := Storehouse_2'Address - Storehouse_1'Address;
      -- Address - Address = Offset
      -- Note that House_Offset may be a negative value

      if House_Offset + Storehouse_1'Address /= Storehouse_2'Address then
      -- Offset + Address = Address
        Report.Failed ("Storage arithmetic non-linear O+A");
      end if;

      if Storehouse_1'Address + House_Offset /= Storehouse_2'Address then
      -- Address + Offset = Address
        Report.Failed ("Storage arithmetic non-linear A+O");
      end if;

      if Storehouse_2'Address - House_Offset /= Storehouse_1'Address then
      -- Address - Offset = Address
        Report.Failed ("Storage arithmetic non-linear A-O");
      end if;

      if (Storehouse_2'Address mod abs(House_Offset) > abs(House_Offset)) then
      -- "mod"( Address, Storage_Offset)
        Report.Failed("Mod arithmetic");
      end if;


      if Storehouse_1'Address
         /= To_Address(To_Integer(Storehouse_1'Address)) then
        Report.Failed("To_Address, To_Integer not symmetric");
      end if;

  end CD71;


begin  -- Main test procedure.

   Report.Test ("CD70001", "Check package System" );

   CD70;

   CD71;

  Report.Result;

end CD70001;
