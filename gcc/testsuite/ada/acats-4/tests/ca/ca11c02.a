-- CA11C02.A
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
--      Check that primitive operations declared in a child package
--      override operations declared in ancestor packages, and that
--      operations on class-wide types defined in the ancestor packages 
--      dispatch as appropriate to these overriding implementations. 
--
-- TEST DESCRIPTION:
--
--      This test builds on the foundation code file (FA11C00) that contains
--      a parent package, child package, and grandchild package.  The parent
--      package declares a tagged type and primitive operation.  The child
--      package extends the type, and overrides the primitive operation. The
--      grandchild package does the same.  
--
--      The test procedure "withs" the grandchild package, and receives
--      visibility to all of its ancestor packages, types and operations.
--      A procedure with a formal class-wide parameter is defined that will
--      allow for dispatching calls to the overridden primitive operations,
--      based on the specific type of the actual parameter.  The primitive
--      operations provide a string value to update a global string array
--      variable.  Calls to the local procedure are made, with objects of each
--      of the tagged types as parameters, and the global variable is finally 
--      examined to ensure that the correct version of primitive operation was 
--      dispatched correctly.
--
-- TEST FILES:
--      This test depends on the following foundation code:
--
--         FA11C00.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with FA11C00_0.FA11C00_1.FA11C00_2;    -- Package Animal.Mammal.Primate
with Report;

procedure CA11C02 is

   package Animal_Package  renames FA11C00_0;
   package Mammal_Package  renames FA11C00_0.FA11C00_1;
   package Primate_Package renames FA11C00_0.FA11C00_1.FA11C00_2;

   Max_Animals : constant := 3;

   type Data_Base_Type is array (1 .. Max_Animals) of String (1 .. 37);

   Zoo_Data_Base : Data_Base_Type := (others => (others => ' '));
                                      -- Global variable.

   Macaw : Animal_Package.Animal   := (Common_Name => "Scarlet Macaw       ", 
                                       Weight      => 2);

   Manatee : Mammal_Package.Mammal := (Common_Name => "Southern Manatee    ",
                                       Weight      => 230,
                                       Hair_Color  => Mammal_Package.Brown);

   Lemur : Primate_Package.Primate := 
              (Common_Name => "Ring-Tailed Lemur   ",
               Weight      => 5,
               Hair_Color  => Mammal_Package.Black,
               Habitat     => Primate_Package.Arboreal);
begin

   Report.Test ("CA11C02", "Check that primitive operations declared "   &
                           "in a child package override operations declared " &
                           "in ancestor packages, and that operations " &
                           "on class-wide types defined in the ancestor " &
                           "packages dispatch as appropriate to these " &
                           "overriding implementations");

   declare

      use Animal_Package, Mammal_Package, Primate_Package;

      -- The following procedure updates the global variable Zoo_Data_Base.

      procedure Enter_Data (A : Animal'Class; I : Integer) is
      begin
         Zoo_Data_Base (I) := Image (A);  
      end Enter_Data;              

   begin

      -- Verify initial test conditions.

      if not (Zoo_Data_Base(1)(1..6) = "      ")
         or not
             (Zoo_Data_Base(2)(1..6) = "      ")
         or not 
             (Zoo_Data_Base(3)(1..6) = "      ")
      then
         Report.Failed ("Initial condition failure");
      end if;


      -- Enter data from all three animals into the zoo database.

      Enter_Data (Macaw, 1);                 -- First entry in database.
      Enter_Data (A => Manatee, I => 2);     -- Second entry.
      Enter_Data (Lemur, I => 3);            -- Third entry. 

      -- Verify the correct version of the overridden function Image was used
      -- for entering the specific data.  

      if not (Zoo_Data_Base(1)(1 .. 6)   = "Animal") 
        or not
             (Zoo_Data_Base(1)(26 .. 30) = "Macaw") 
        then
           Report.Failed ("Incorrect version of Image for parent type");
      end if;

      if not (Zoo_Data_Base(2)(1 .. 6)   = "Mammal"
        and 
          Zoo_Data_Base(2)(27 .. 33) = "Manatee") 
        then
           Report.Failed ("Incorrect version of Image for child type");
      end if;

      if not ((Zoo_Data_Base(3)(1 .. 7)   = "Primate")
        and 
          (Zoo_Data_Base(3)(30 .. 34) = "Lemur"))
        then
           Report.Failed ("Incorrect version of Image for grandchild type");
      end if;

   end;

   Report.Result;

end CA11C02;
