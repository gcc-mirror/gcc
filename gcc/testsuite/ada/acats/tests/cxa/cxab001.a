-- CXAB001.A
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
--      Check that the operations defined in package Wide_Text_IO allow for 
--      the input/output of Wide_Character and Wide_String data.
--
-- TEST DESCRIPTION:
--      This test is designed to exercise the components of the Wide_Text_IO
--      package, including the Put/Get utilities for Wide_Characters and
--      Wide_String objects.
--      The test utilizes the Put and Get procedures defined for
--      Wide_Characters, as well as the Put, Get, Put_Line, and Get_Line
--      procedures defined for Wide_Strings.  In addition, many of the 
--      additional subprograms found in package Wide_Text_IO are used in this
--      test.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations capable of supporting
--      external Wide_Text_IO files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      26 Feb 97   CTA.PWB Allowed for non-support of some IO operations.
--!

with Ada.Wide_Text_IO;
with Report;

procedure CXAB001 is

   Filter_File     : Ada.Wide_Text_IO.File_Type;
   Filter_Filename : constant String :=
                              Report.Legal_File_Name ( Nam => "CXAB001" );
   Incomplete : exception;


begin

   Report.Test ("CXAB001", "Check that the operations defined in package " &
                           "Wide_Text_IO allow for the input/output of "   &
                           "Wide_Character and Wide_String data");


   Test_for_Wide_Text_IO_Support:
   begin

      -- An implementation that does not support Wide_Text_IO in a particular
      -- environment will raise Use_Error on calls to various
      -- Wide_Text_IO operations.  This block statement encloses a call to
      -- Create, which should raise an exception in a non-supportive 
      -- environment.  This exception will be handled to produce a
      -- Not_Applicable result.

      Ada.Wide_Text_IO.Create (File => Filter_File,                 -- Create.
                               Mode => Ada.Wide_Text_IO.Out_File,
                               Name => Filter_Filename);

   exception

       when Ada.Wide_Text_IO.Use_Error | Ada.Wide_Text_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Wide_Text_IO" );
          raise Incomplete;

   end Test_for_Wide_Text_IO_Support;

   Operational_Test_Block:
   declare

      First_String  : constant Wide_String := "Somewhere ";
      Second_String : constant Wide_String := "Over The ";
      Third_String  : constant Wide_String := "Rainbow";
      Current_Char  : Wide_Character       := ' ';

   begin

      Enter_Data_In_File:
      declare
         Pos                 : Natural := 1;
         Bad_Character_Found : Boolean := False;
      begin
         -- Use the Put procedure defined for Wide_Character data to 
         -- write all of the wide characters of the First_String into 
         -- the file individually, followed by a call to New_Line.

         while Pos <= First_String'Length loop
            Ada.Wide_Text_IO.Put (Filter_File, First_String (Pos));  -- Put.
            Pos := Pos + 1;
         end loop;
         Ada.Wide_Text_IO.New_Line (Filter_File);               -- New_Line.

         -- Reset to In_File mode and read file contents, using the Get
         -- procedure defined for Wide_Character data.
         Reset1:
         begin
            Ada.Wide_Text_IO.Reset (Filter_File,                      -- Reset.
                                    Ada.Wide_Text_IO.In_File);   
         exception
            when Ada.Wide_Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Wide_Text_IO" );
               raise Incomplete;
         end Reset1;
                                      
         Pos := 1;
         while Pos <= First_String'Length loop
            Ada.Wide_Text_IO.Get (Filter_File, Current_Char);        -- Get.
            -- Verify the wide character against the original string.
            if Current_Char /= First_String(Pos) then
               Bad_Character_Found := True;
            end if;
            Pos := Pos + 1;
         end loop;

         if Bad_Character_Found then
            Report.Failed ("Incorrect Wide_Character read from file - 1");
         end if;

            -- Following user file/string processing, the Wide_String data 
            -- of the Second_String and Third_String Wide_String objects are 
            -- appended to the file.
            -- The Put procedure defined for Wide_String data is used to 
            -- transfer the Second_String, followed by a call to New_Line.  
            -- The Put_Line procedure defined for Wide_String data is used 
            -- to transfer the Third_String.
         Reset2:
         begin
            Ada.Wide_Text_IO.Reset    (Filter_File,                   -- Reset.
                                       Ada.Wide_Text_IO.Append_File);

         exception
            when Ada.Wide_Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to Append_File not supported for Wide_Text_IO" );
               raise Incomplete;
         end Reset2;

         Ada.Wide_Text_IO.Put      (Filter_File, Second_String);     -- Put.
         Ada.Wide_Text_IO.New_Line (Filter_File);               -- New_Line.

         Ada.Wide_Text_IO.Put_Line (Filter_File, Third_String); -- Put_Line.
         Ada.Wide_Text_IO.Close    (Filter_File);               -- Close.

      exception

         when Incomplete =>
           raise; 

         when others => 
            Report.Failed ("Exception in Enter_Data_In_File block");
            raise;
 
      end Enter_Data_In_File;

         ---

      Filter_Block:
      declare

         Pos          : Positive := 1;
         TC_String2   : Wide_String (1..Second_String'Length);
         TC_String3   : Wide_String (1..Third_String'Length);
         Last         : Natural  := Natural'First;

      begin

         Ada.Wide_Text_IO.Open (Filter_File,                       -- Open.
                                Ada.Wide_Text_IO.In_File,
                                Filter_Filename);


         -- Read the data of the First_String from the file, using the
         -- Get procedure defined for Wide_Character data.
         -- Verify that the character corresponds to the data originally
         -- written to the file.

         while Pos <= First_String'Length loop
            Ada.Wide_Text_IO.Get (Filter_File, Current_Char);       -- Get.
            if Current_Char /= First_String(Pos) then
               Report.Failed 
                 ("Incorrect Wide_Character read from file - 2");
            end if;
            Pos := Pos + 1;
         end loop;
          
         -- The first line of the file has been read, move to the second.
         Ada.Wide_Text_IO.Skip_Line (Filter_File);             -- Skip_Line.

         -- Read the Wide_String data from the second and third lines of
         -- the file.
         Ada.Wide_Text_IO.Get       (Filter_File, TC_String2); -- Get.
         Ada.Wide_Text_IO.Skip_Line (Filter_File);             -- Skip_Line.
         Ada.Wide_Text_IO.Get_Line  (Filter_File,              -- Get_Line.
                                     TC_String3, Last);        

         -- Verify data of second and third strings.
         if TC_String2 /= Second_String then
            Report.Failed ("Incorrect Wide_String read from file - 1");
         end if;
         if TC_String3 /= Third_String then
            Report.Failed ("Incorrect Wide_String read from file - 2");
         end if;

         -- The file should now be at EOF.                 
         if not Ada.Wide_Text_IO.End_Of_File (Filter_File) then      -- EOF.
            Report.Failed ("File not empty following filtering");
         end if;

      exception
         when others => 
            Report.Failed ("Exception in Filter_Block");
            raise;
      end Filter_Block;

   exception

      when Incomplete =>
         raise; 
      when others => 
         Report.Failed ("Exception raised in Operational Test Block");
      
   end Operational_Test_Block;

   Deletion:
   begin
      if Ada.Wide_Text_IO.Is_Open (Filter_File) then            -- Is_Open.
         Ada.Wide_Text_IO.Delete (Filter_File);                 -- Delete.
      else
         Ada.Wide_Text_IO.Open (Filter_File,                    -- Open.
                                Ada.Wide_Text_IO.Out_File, 
                                Filter_Filename);
         Ada.Wide_Text_IO.Delete (Filter_File);                 -- Delete.
      end if;
   exception
      when others =>
         Report.Failed ("Delete not properly implemented for Wide_Text_IO");
   end Deletion;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAB001;
