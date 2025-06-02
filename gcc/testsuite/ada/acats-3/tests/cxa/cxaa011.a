-- CXAA011.A
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
--      Check that the operations of Text_IO.Enumeration_IO perform correctly
--      on files of Append_File mode, for instantiations using 
--      enumeration types.  Check that Enumeration_IO procedures Put and Get 
--      properly transfer enumeration data to/from data files.
--      Check that the formatting parameters available in the package can
--      be used and modified successfully in the storage and retrieval of data.
--      
-- TEST DESCRIPTION:
--      This test is designed to simulate an environment where a data file
--      that holds enumeration type information is reset from it current mode
--      to allow the appending of data to the end of the   This process
--      of Reset/Write can be repeated as necessary.  All data written
--      to the file is verified for accuracy when retrieved from the file.
--
--      This test verifies issues of resetting a file created in Out_File mode
--      to Append_File mode, resetting from Append_File mode to In_File mode, 
--      as well as a variety of Text_IO and Enumeration_IO predefined 
--      subprograms.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable only to implementations that support text
--      files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--!

with Ada.Text_IO;
with Report;

procedure CXAA011 is
   use Ada;

   Status_Log          : Text_IO.File_Type;
   Status_Log_Filename : constant String :=
                           Report.Legal_File_Name ( Nam => "CXAA011" );
   Incomplete : exception;

begin

   Report.Test ("CXAA011", "Check that the operations of "                 &
                           "Text_IO.Enumeration_IO operate correctly for " &
                           "files with mode Append_File");

   Test_for_Text_IO_Support:
   begin

      -- An implementation that does not support Text_IO in a particular
      -- environment will raise Use_Error on calls to various
      -- Text_IO operations.  This block statement encloses a call to
      -- Create, which should raise the exception in a non-supportive 
      -- environment.  This exception will be handled to produce a
      -- Not_Applicable result.

      Text_IO.Create (File => Status_Log,
                      Mode => Text_IO.Out_File,
                      Name => Status_Log_Filename);
   exception

       when Text_IO.Use_Error | Text_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Text_IO" );
          raise Incomplete;

   end Test_for_Text_IO_Support;


   Operational_Test_Block:
   declare

      type Days_In_Week is (Monday, Tuesday, Wednesday, Thursday, Friday, 
                            Saturday, Sunday);
      type Hours_In_Day is (A0000, A0600, P1200, P0600); -- Six hour 
                                                         -- blocks.
      type Status_Type  is (Operational, Off_Line);

      type Status_Record_Type is record
         Day    : Days_In_Week;
         Hour   : Hours_In_Day;
         Status : Status_Type;
      end record;

      Morning_Reading : Status_Record_Type := 
                          (Wednesday, A0600, Operational);
      Evening_Reading : Status_Record_Type := 
                          (Saturday,  P0600, Off_Line);

      package Day_IO    is new Text_IO.Enumeration_IO (Days_In_Week);
      package Hours_IO  is new Text_IO.Enumeration_IO (Hours_In_Day);
      package Status_IO is new Text_IO.Enumeration_IO (Status_Type);


      -- The following function simulates the hourly recording of equipment
      -- status.

      function Record_Status (Reading : Status_Record_Type) 
      return Boolean is
         use Text_IO;  -- To provide visibility to type Type_Set and
                       -- enumeration literal Upper_Case.
      begin
         Day_IO.Put       (File => Status_Log, 
                           Item => Reading.Day, 
                           Set  => Type_Set'(Upper_Case));
         Hours_IO.Put     (Status_Log, Reading.Hour, 7);
         Status_IO.Put    (Status_Log, Reading.Status, 
                           Width => 8, Set => Lower_Case);
         Text_IO.New_Line (Status_Log);
         return (True);
      exception
         when others => return False;
      end Record_Status;

   begin

      -- The usage scenario intended is as follows:
      --    File is created.
      --    Unrelated/unknown file processing occurs.
      --    On six hour intervals, file is reset to Append_File mode.
      --    Data is appended to file.
      --    Unrelated/unknown file processing resumes.
      --    Reset/Append process is repeated.

      Reset1:
      begin 
        Text_IO.Reset (Status_Log,                           -- Reset to 
                        Text_IO.Append_File);                 -- Append mode.
      exception
         when Text_IO.Use_Error =>
            Report.Not_Applicable
               ( "Reset to Append_File not supported for Text_IO" );
            raise Incomplete;
      end Reset1;

      Day_IO.Default_Width := Days_In_Week'Width + 5;      -- Default values
                                                           -- are modifiable.

      if not Record_Status (Morning_Reading) then          -- Enter data.
         Report.Failed ("Exception occurred during data file update");
      end if;

      Reset2:
      begin
         Text_IO.Reset (Status_Log,                           -- Reset to 
                        Text_IO.Append_File);                 -- Append mode.
      exception
         when Text_IO.Use_Error =>
            Report.Not_Applicable
               ( "Reset to Append_File not supported for Text_IO" );
            raise Incomplete;
      end Reset2;

      if not Record_Status (Evening_Reading) then          -- Enter data.
         Report.Failed ("Exception occurred during data file update");
      end if;

      Test_Verification_Block:                             
      declare                                              
         TC_Reading1   : Status_Record_Type;
         TC_Reading2   : Status_Record_Type;
      begin                                                

         Reset3:
         begin
            Text_IO.Reset (Status_Log, Text_IO.In_File);      -- Reset for
                                                              -- reading.
         exception
            when Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Text_IO" );
               raise Incomplete;
         end Reset3;

         Day_IO.Get    (Status_Log, TC_Reading1.Day);      -- Read data from
         Hours_IO.Get  (Status_Log, TC_Reading1.Hour);     -- first record.
         Status_IO.Get (Status_Log, TC_Reading1.Status);
         Text_IO.Skip_Line (Status_Log);

         -- Verify the data read from the file.  Compare with the 
         -- record that was originally entered into the file.

         if (TC_Reading1 /= Morning_Reading) then
            Report.Failed ("Data error on reading first record");
         end if;

         Day_IO.Get    (Status_Log, TC_Reading2.Day);      -- Read data from
         Hours_IO.Get  (Status_Log, TC_Reading2.Hour);     -- second record.
         Status_IO.Get (Status_Log, TC_Reading2.Status);
         Text_IO.Skip_Line (Status_Log);

         -- Verify all of the data fields read from the file.  Compare
         -- with the values that were originally entered into the file.

         if (TC_Reading2.Day    /= Evening_Reading.Day)    or
            (TC_Reading2.Hour   /= Evening_Reading.Hour)   or
            (TC_Reading2.Status /= Evening_Reading.Status) then
            Report.Failed ("Data error on reading second record");
         end if;

      exception
        when Incomplete =>
           raise; 
         when others => 
            Report.Failed ("Error raised during data verification");
      end Test_Verification_Block;

   exception
      when Incomplete =>
         raise; 
      when others => 
         Report.Failed ("Exception in Text_IO.Enumeration_IO processing");
   end Operational_Test_Block;

   Final_Block:
   begin
      -- Delete the external file.
      if Text_IO.Is_Open (Status_Log) then
         Text_IO.Delete (Status_Log);   
      else
         Text_IO.Open (Status_Log, Text_IO.Out_File, Status_Log_Filename);
         Text_IO.Delete (Status_Log);   
      end if;             
   exception
      when Text_IO.Use_Error =>
         Report.Failed
            ( "Delete not properly implemented for Text_IO" );

   end Final_Block;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAA011;
