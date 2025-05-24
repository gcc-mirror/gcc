-- C3900053.AM
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
--      Check that a private tagged type declared in a package specification
--      may be extended with a private extension in a different package
--      specification, and that this private extension may in turn be extended
--      by a private extension in a third package.
--
--      Check that each derivative inherits the user-defined primitive
--      subprograms of its parent (including those that its parent inherited),
--      that it may override these inherited primitive subprograms, and that it
--      may also declare its own primitive subprograms.
--
--      Check that type conversion is defined from a type extension to its
--      parent, and that this parent itself may be a type extension.
--
-- TEST DESCRIPTION:
--      Declare a root tagged private type and two associated primitive
--      subprograms in a package specification. Declare operations to verify
--      the correctness of the components. Declare operations which return
--      values of the type's private components, and which will be
--      inherited by later derivatives.
-- 
--      Extend the root type with a private extension in a second package
--      specification. Declare a new primitive subprogram for the extension,
--      and override one of the two inherited subprograms. Within the
--      overriding subprogram, utilize type conversion to call the parent's
--      implementation of the same subprogram. Also within the overriding
--      subprogram, call the new primitive subprogram and each inherited
--      subprogram. Declare operations of the private extension which
--      override the verification operations of its parent. Declare operations
--      of the private extension which return values of the extension's
--      private components, and which will be inherited by later derivatives.
--      
--      Extend the extension with a private extension in a third package
--      specification. Declare a new primitive subprogram for this private
--      extension, and override one of the three inherited subprograms.
--      Within the overriding subprogram, utilize type conversion to call the
--      parent's implementation of the same subprogram. Also within the
--      overriding subprogram, call the new primitive subprogram and each
--      inherited subprogram. Declare operations of the private extension
--      which override the verification operations of its parent.
-- 
--      In the main program, declare objects of the root tagged type and
--      the two type extensions. For each object, call the overriding
--      subprogram, and verify the correctness of the components by calling
--      the verification operations.
--
-- TEST FILES:
--      This test consists of the following files:
--
--         C3900050.A
--         C3900051.A
--         C3900052.A
--      => C3900053.AM
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 May 96   SAIC    ACVC 2.1: Modified prologue.
--
--!

with Report;

with C3900050; -- Basic alert abstraction.
with C3900051; -- Extended alert abstraction.
with C3900052; -- Further extended alert abstraction.

use  C3900050; -- Primitive operations of Alert_Type directly visible.

procedure C3900053 is
begin

   Report.Test ("C390005", "Primitive operation inheritance by type " &
                "extensions: root type is private; all extensions are " &
                "private and declared in different packages");


   ALERT_SUBTEST: -------------------------------------------------------------

      declare
         Alarm : C3900050.Alert_Type;     -- Root tagged private type.
      begin
         if not Initial_Values_Okay (Alarm) then
            Report.Failed ("Wrong initial values for Alert_Type");
         end if;

         Handle (Alarm);

         if Bad_Final_Values (Alarm) then
            Report.Failed ("Wrong values for Alert_Type after Handle");
         end if;
      end Alert_Subtest;


   -- Check intermediate display counts:

   if C3900050.Display_Count_For (Null_Device) /= 1 or
      C3900050.Display_Count_For (Teletype)    /= 0 or
      C3900050.Display_Count_For (Console)     /= 0 or
      C3900050.Display_Count_For (Big_Screen)  /= 0
   then
      Report.Failed ("Wrong display counts after Alert_Type");
   end if;


   LOW_ALERT_SUBTEST: ---------------------------------------------------------

      declare
         Low_Alarm : C3900051.Low_Alert_Type; -- Priv. ext. of tagged type.
         use C3900051; -- Primitive operations of extension directly visible.
      begin
         if not Initial_Values_Okay (Low_Alarm) then
            Report.Failed ("Wrong initial values for Low_Alert_Type");
         end if;

         Handle (Low_Alarm);

         if Bad_Final_Values (Low_Alarm) then
            Report.Failed ("Wrong values for Low_Alert_Type after Handle");
         end if;
      end Low_Alert_Subtest;


   -- Check intermediate display counts:

   if C3900050.Display_Count_For /= (Null_Device => 2,
                                     Teletype    => 1,
                                     Console     => 0,
                                     Big_Screen  => 0)
   then
      Report.Failed ("Wrong display counts after Low_Alert_Type");
   end if;


   MEDIUM_ALERT_SUBTEST: ------------------------------------------------------

      declare
         Medium_Alarm : C3900052.Medium_Alert_Type; -- Priv. ext. of extension.
         use C3900052; -- Primitive operations of extension directly visible.
      begin
         if not Initial_Values_Okay (Medium_Alarm) then
            Report.Failed ("Wrong initial values for Medium_Alert_Type");
         end if;

         Handle (Medium_Alarm);

         if Bad_Final_Values (Medium_Alarm) then
            Report.Failed ("Wrong values for Medium_Alert_Type after Handle");
         end if;
      end Medium_Alert_Subtest;


   -- Check final display counts:

   if C3900050.Display_Count_For /= (Null_Device => 3,
                                     Teletype    => 2,
                                     Console     => 1,
                                     Big_Screen  => 0)
   then
      Report.Failed ("Wrong display counts after Medium_Alert_Type");
   end if;


   Report.Result;

end C3900053;
