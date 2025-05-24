-- CA11C01.A
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
--      Check that when primitive operations declared in a child package
--      override operations declared in ancestor packages, a client of the
--      child package inherits the operations correctly.
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
--      Three procedures, each with a formal parameter of a specific type are
--      defined.  Each of these invokes a particular version of the overridden
--      primitive operation Image.  Calls to these local procedures are made, 
--      with objects of each of the tagged types as parameters, and the global 
--      variable is finally examined to ensure that the correct version of 
--      primitive operation was inherited by the client and invoked by the 
--      call.
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

procedure CA11C01 is

   package Animal_Package  renames FA11C00_0;
   package Mammal_Package  renames FA11C00_0.FA11C00_1;
   package Primate_Package renames FA11C00_0.FA11C00_1.FA11C00_2;

   Max_Animals : constant := 3;

   subtype Data_String is String (1 .. 37);
   type Data_Base_Type is array  (1 .. Max_Animals) of Data_String;

   Zoo_Data_Base : Data_Base_Type := (others => (others => ' '));
                                      -- Global variable.

   Salmon   : Animal_Package.Animal := (Common_Name => "Chinook Salmon      ", 
                                        Weight      => 10);

   Platypus : Mammal_Package.Mammal := (Common_Name => "Tasmanian Platypus  ",
                                        Weight      => 13,
                                        Hair_Color  => Mammal_Package.Brown);

   Orangutan : Primate_Package.Primate := 
                 (Common_Name => "Sumatran Orangutan  ",
                  Weight      => 220,
                  Hair_Color  => Mammal_Package.Red,
                  Habitat     => Primate_Package.Arboreal);
begin

   Report.Test ("CA11C01", "Check that when primitive operations declared "   &
                           "in a child package override operations declared " &
                           "in ancestor packages, a client of the child "     &
                           "package inherits the operations correctly");

   declare

      use Animal_Package, Mammal_Package, Primate_Package;

      -- The function Image has been overridden in the child and grandchild
      -- packages, but the client has inherited all versions of the function,
      -- and can successfully use them to enter data into the database.
      -- Each of the following procedures updates the global variable 
      -- Zoo_Data_Base.

      procedure Enter_Animal_Data (A : Animal; I : Integer) is
      begin
         Zoo_Data_Base (I) := Image (A);  
      end Enter_Animal_Data;              

      procedure Enter_Mammal_Data (M : Mammal; I : Integer) is
      begin
         Zoo_Data_Base (I) := Image (M);  
      end Enter_Mammal_Data;

      procedure Enter_Primate_Data (P : Primate; I : Integer) is
      begin
         Zoo_Data_Base (I) := Image (P);  
      end Enter_Primate_Data;

   begin

      -- Verify initial test conditions.

      if not (Zoo_Data_Base(1)(1..6) = "      ") 
         or else
             (Zoo_Data_Base(2)(1..6) /= "      ") 
         or else
             (Zoo_Data_Base(3)(1..6) /= "      ")
      then
         Report.Failed ("Initial condition failure");
      end if;


      -- Enter data from all three animals into the zoo database.

      Enter_Animal_Data  (A => Salmon,    I => 1);  -- First entry in database.
      Enter_Mammal_Data  (M => Platypus,  I => 2);  -- Second entry.
      Enter_Primate_Data (P => Orangutan, I => 3);  -- Third entry. 

      -- Verify the correct version of the overridden function Image was used
      -- for entering the specific data.  

      if  Zoo_Data_Base(1)(1 .. 6) /= "Animal"
        or else
          Zoo_Data_Base(1)(26 .. 31) /= "Salmon"
        then
           Report.Failed ("Incorrect version of Image for parent type");
      end if;

      if (Zoo_Data_Base(2)(1 .. 6) /= "Mammal") 
        or
          (Zoo_Data_Base(2)(28 .. 35) /= "Platypus") 
        then
           Report.Failed ("Incorrect version of Image for child type");
      end if;

      if ((Zoo_Data_Base(3)(1 .. 7) /= "Primate") 
        or 
          (Zoo_Data_Base(3)(27 .. 35) /= "Orangutan"))
        then
           Report.Failed ("Incorrect version of Image for grandchild type");
      end if;

   end;


   Report.Result;

end CA11C01;
