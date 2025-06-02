-- CXA8002.A
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
--      Check that resetting a file using mode Append_File allows for the 
--      writing of elements to the file starting after the last element in
--      the file.  
--      Check that the result of function Name can be used on a subsequent
--      reopen of the file.
--      Check that a mode change occurs on reset of a file to/from mode 
--      Append_File.
--
-- TEST DESCRIPTION:
--      This test simulates the read/write of data from/to an individual 
--      sequential file. New data can be appended to the end of the existing 
--      file, and the same file can be reset to allow reading of data from 
--      the file.  This process can occur multiple times.
--      When the mode of the file is changed with a Reset, the current mode
--      value assigned to the file is checked using the result of function
--      Mode.  This, in conjunction with the read/write operations, verifies
--      that a mode change has taken place on Reset.
--      
--      An expected common usage of the scenarios found in this test would
--      be a case where a single data file is kept open continuously, being
--      reset for read/append of data.  For systems that do not support a
--      direct form of I/O, this would allow for efficient use of a sequential
--      I/O file.
--
-- APPLICABILITY CRITERIA: 
--      Applicable to all systems capable of supporting IO operations on 
--      external Sequential_IO files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Feb 97   PWB.CTA Fixed handling for file non-support and Reset
--                          non-support.
--!

with Sequential_IO;
with Report;

procedure CXA8002 is
   subtype Employee_Data is String (1 .. 11);
   package Data_IO       is new Sequential_IO (Employee_Data); 

   Employee_Data_File : Data_IO.File_Type;
   Employee_Filename  : constant String := 
     Report.Legal_File_Name (Nam => "CXA8002");

   Incomplete         : exception;

begin

   Report.Test ("CXA8002", "Check that resetting a file using mode "  &
                           "Append_File allows for the writing of "   &
                           "elements to the file starting after the " &
                           "last element in the file");  

   Test_for_Sequential_IO_Support:
   begin

      -- An implementation that does not support Sequential_IO in a particular
      -- environment will raise Use_Error or Name_Error on calls to various
      -- Sequential_IO operations.  This block statement encloses a call to
      -- Create, which should produce an exception in a non-supportive 
      -- environment.  These exceptions will be handled to produce a
      -- Not_Applicable result.

      Data_IO.Create (File => Employee_Data_File,    -- Create file in 
                      Mode => Data_IO.Append_File,   -- mode Append_File.
                      Name => Employee_Filename);

      --
      -- The following portion of code demonstrates the fact that a sequential
      -- file can be created in Append_File mode, and that data can be written
      -- to the file.
      --

   exception
      when Data_IO.Use_Error | Data_IO.Name_Error =>
         Report.Not_Applicable
            ( "Sequential files not supported - Create as Append_File");
         raise Incomplete;
   end Test_for_Sequential_IO_Support;
      Operational_Test_Block:
      declare
         Blank_Data         : constant Employee_Data := "           ";
         Employee_1         : constant Employee_Data := "123-45-6789";
         Employee_2         :          Employee_Data := "987-65-4321";

         -- Note: Artificial numerical data chosen above to prevent any
         --       unintended similarity with persons alive or dead.

         TC_Employee_Data   :          Employee_Data := Blank_Data;


         function TC_Mode_Selection (Selector : Integer) 
           return Data_IO.File_Mode is
         begin
            case Report.Ident_Int(Selector) is
               when 1       => return Data_IO.In_File;
               when 2       => return Data_IO.Out_File;
               when others  => return Data_IO.Append_File;
            end case;
         end TC_Mode_Selection;

         Employee_Filename : constant String :=       -- Use function Name to 
           Data_IO.Name (File => Employee_Data_File); -- store filename in 
                                                      -- string variable.
      begin

         Data_IO.Write (File => Employee_Data_File,   -- Write initial data 
                        Item => Employee_1);          -- entry to file.

         --
         -- The following portion of code demonstrates that a sequential file
         -- can be reset to various file modes, including Append_File mode, 
         -- allowing data to be added to the end of the file.
         --
         begin
           Data_IO.Reset (File => Employee_Data_File,   -- Reset file with 
                          Mode => Data_IO.In_File);     -- mode In_File.
         exception
           when Data_IO.Use_Error =>
              Report.Not_Applicable
                ("Reset to In_File not supported for Sequential_IO");
              raise Incomplete;
           when others =>
              Report.Failed 
                ("Unexpected exception on Reset to In_File (Sequential_IO)");
              raise Incomplete;
         end;
         if Data_IO."="(Data_IO.Mode (Employee_Data_File), 
                        TC_Mode_Selection (1)) then   -- Compare In_File mode
                                                      -- Reset successful, 
            Data_IO.Read (File => Employee_Data_File, -- now verify file data.
                          Item => TC_Employee_Data);
      
            if ((TC_Employee_Data (1 .. 7)  /= "123-45-") or
                (TC_Employee_Data (5 .. 11) /= "45-6789")) then
               Report.Failed ("Data read error");
            end if;

         else
            Report.Failed ("File mode not changed by Reset");
         end if;

         --
         -- Simulate appending data to a file that has previously been written 
         -- to and read from.
         --
         begin
           Data_IO.Reset (File => Employee_Data_File,   -- Reset file with 
                          Mode => Data_IO.Append_File); -- mode Append_File.
         exception
           when Data_IO.Use_Error =>
             Report.Not_Applicable
               ("Reset to Append_File not supported for Sequential_IO");
             raise Incomplete;
           when others =>
             Report.Failed 
              ("Unexpected exception on Reset to Append_File (Sequential_IO)");
              raise Incomplete;
         end;

         if Data_IO.Is_Open (Employee_Data_File) then -- File remains open
                                                      -- following Reset to 
                                                      -- Append_File mode?

            if Data_IO."=" (Data_IO.Mode (Employee_Data_File), 
                            TC_Mode_Selection (3)) then   -- Compare to
                                                          -- Append_File mode.
               Data_IO.Write (File => Employee_Data_File, -- Write additional
                              Item => Employee_2);        -- data to file.
            else
               Report.Failed ("File mode not changed by Reset");
            end if;

         else
            Report.Failed 
              ("File status not Open following Reset to Append mode");
         end if;

         Data_IO.Close (Employee_Data_File);


         Test_Verification_Block:
         begin

            Data_IO.Open (File => Employee_Data_File, -- Reopen file, using 
                          Mode => Data_IO.In_File,    -- previous result of
                          Name => Employee_Filename); -- function Name.

            TC_Employee_Data := Blank_Data;           -- Clear record field.
            Data_IO.Read (Employee_Data_File,         -- Read first record,
                          TC_Employee_Data);          -- check ordering of 
                                                      -- records.

            if not ((TC_Employee_Data (1 .. 3)  = "123") and then
                    (TC_Employee_Data (4 .. 11) = "-45-6789")) then
               Report.Failed ("Data read error - first record");
            end if;

            TC_Employee_Data := Blank_Data;           -- Clear record field.
            Data_IO.Read (Employee_Data_File,         -- Read second record,
                          TC_Employee_Data);          -- check for ordering of 
                                                      -- records.

            if ((TC_Employee_Data (1 .. 6)  /= "987-65") or else
                not (TC_Employee_Data (3 .. 11) = "7-65-4321")) then
               Report.Failed ("Data read error - second record");
            end if;

            -- Check that only two items were written to the file.
            if not Data_IO.End_Of_File(Employee_Data_File) then
               Report.Failed("Incorrect number of records in file");
            end if;

         exception
      
            when Data_IO.End_Error =>             -- If two items not in 
                                                  -- file (data overwritten),
                                                  -- then fail.
               Report.Failed ("Incorrect number of record elements in file");

            when others => 
               Report.Failed ("Error raised during data verification");

         end Test_Verification_Block;

      exception

         when others => 
            Report.Failed("Exception raised during Sequential_IO processing");

      end Operational_Test_Block;

      Final_Block:
      begin
        -- Check that file is open prior to deleting it.
        if Data_IO.Is_Open(Employee_Data_File) then
           Data_IO.Delete (Employee_Data_File);         
        else
           Data_IO.Open(Employee_Data_File, 
                        Data_IO.In_File, 
                        Employee_Filename);
           Data_IO.Delete (Employee_Data_File);          
        end if;
      exception
         when others =>
            Report.Failed ("Sequential_IO Delete not properly supported");
      end Final_Block;

   Report.Result;

exception
  when Incomplete =>
    Report.Result;
  when others =>
    Report.Failed ("Unexpected exception");
    Report.Result;
end CXA8002;
