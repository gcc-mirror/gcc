-- CA11C03.A
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
--      Check that when a child unit is "withed", visibility is obtained to 
--      all ancestor units named in the expanded name of the "withed" child
--      unit.  Check that when the parent unit is "used", the simple name of 
--      a "withed" child unit is made directly visible.
--
-- TEST DESCRIPTION:
--      To satisfy the first part of the objective, various references are
--      made to types and functions declared in the ancestor packages of the
--      foundation code package hierarchy.  Since the grandchild library unit
--      package has been "withed" by this test, the visibility of these 
--      components demonstrates that visibility of the ancestor package names
--      is provided when the expanded name of a child library unit is "withed". 
--      
--      The declare block in the test program includes a "use" clause of the
--      parent package (FA11C00_0.FA11C00_1) of the "withed" child package.  
--      As a result, the simple name of the child package (FA11C00_2) is
--      directly visible.  The type and function declared in the child 
--      package are now visible when qualified with the simple name of the
--      "withed" package (FA11C00_2).
--      
--      This test simulates the formatting of data strings, based on the
--      component fields of a "doubly-extended" tagged record type.
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

with FA11C00_0.FA11C00_1.FA11C00_2;  -- "with" of child library package 
                                     -- Animal.Mammal.Primate.
                                     -- This will be used in conjunction with
                                     -- a "use" of FA11C00_0.FA11C00_1 below 
                                     -- to verify a portion of the objective.
with Report;

procedure CA11C03 is

   Blank_Name_String : constant FA11C00_0.Species_Name_Type := (others => ' ');
                                     -- Visibility of grandparent package.
                                     -- The package FA11C00_0 is visible since
                                     -- it is an ancestor that is mentioned in
                                     -- the expanded name of its "withed"
                                     -- grandchild package.

   Blank_Hair_Color : 
     String (1..FA11C00_0.FA11C00_1.Hair_Color_Type'Width) := (others => ' ');
                                     -- Visibility of parent package.
                                     -- The package FA11C00_0.FA11C00_1 is
                                     -- visible due to the "with" of its 
                                     -- child package.

   subtype Data_String_Type is String (1 .. 60);

   TC_Result_String : Data_String_Type := (others => ' ');

   --

   function Format_Primate_Data (Name : String := Blank_Name_String;  
                                 Hair : String := Blank_Hair_Color) 
     return Data_String_Type is

      Pos                        : Integer := 1;
      Hair_Color_Field_Separator : constant String := " Hair Color: ";

      Result_String              : Data_String_Type := (others => ' '); 

   begin
      Result_String (Pos .. Name'Length) := Name;    -- Enter name at start
                                                     -- of string.
      Pos := Pos + Name'Length;                      -- Increment counter to
                                                     -- next blank position.
      Result_String 
        (Pos .. Pos + Hair_Color_Field_Separator'Length + Hair'Length - 1) :=
        Hair_Color_Field_Separator & Hair;           -- Include hair color data
                                                     -- in result string.
      return (Result_String);                        
   end Format_Primate_Data;


begin

   Report.Test ("CA11C03", "Check that when a child unit is WITHED, "        &
                           "visibility is obtained to all ancestor units "   &
                           "named in the expanded name of the WITHED child " &
                           "unit. Check that when the parent unit is USED, " &
                           "the simple name of a WITHED child unit is made " &
                           "directly visible" );

   declare
      use FA11C00_0.FA11C00_1;    -- This "use" clause will allow direct 
                                  -- visibility to the simple name of
                                  -- package FA11C00_0.FA11C00_1.FA11C00_2,
                                  -- since this child package was "withed" by
                                  -- the main program.

      Tarsier : FA11C00_2.Primate := (Common_Name => "East-Indian Tarsier ",
                                      Weight      => 7,
                                      Hair_Color  => Brown,
                                      Habitat     => FA11C00_2.Arboreal);

                                  -- Demonstrates visibility of package
                                  -- FA11C00_0.FA11C00_1.FA11C00_2.
                                  --
                                  -- Type Primate referenced with the simple
                                  -- name of package FA11C00_2 only.
                                  --
                                  -- Simple name of package FA11C00_2 is 
                                  -- directly visible through "use" of parent.

   begin

      -- Verify that the Format_Primate_Data function will return a blank
      -- filled string when no parameters are provided in the call.

      TC_Result_String := Format_Primate_Data;

      if (TC_Result_String (1 .. 20)  /= Blank_Name_String) then 
         Report.Failed ("Incorrect initialization value from function");
      end if;


      -- Use function Format_Primate_Data to return a formatted data string.

      TC_Result_String := 
        Format_Primate_Data 
         (Name => FA11C00_2.Image (Tarsier),       
                                  -- Function returns a 37 character string 
                                  -- value.
          Hair => Hair_Color_Type'Image(Tarsier.Hair_Color));
                                  -- The Hair_Color_Type is referenced 
                                  -- directly, without package
                                  -- FA11C00_0.FA11C00_1 qualifier.
                                  -- No qualification of Hair_Color_Type is
                                  -- needed due to "use" clause.

                                  -- Note that the result of calling 'Image 
                                  -- with an enumeration type argument 
                                  -- results in an upper-case string.
                                  -- (See conditional statement below.)

      -- Verify the results of the function call.

      if not  (TC_Result_String (1 .. 37) = 
                "Primate Species: East-Indian Tarsier " and then
              TC_Result_String (38 .. 55) =
                " Hair Color: BROWN") then
        Report.Failed ("Incorrect result returned from function call");
      end if;

   end;

   Report.Result;

end CA11C03;
