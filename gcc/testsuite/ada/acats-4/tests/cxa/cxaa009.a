-- CXAA009.A
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
--      Check that the capabilities provided in instantiations of the
--      Ada.Text_IO.Float_IO package operate correctly when the mode of 
--      the file is Append_File. Check that Float_IO procedures Put and Get 
--      properly transfer floating point data to/from data files that are in
--      Append_File mode. Check that the formatting parameters available in 
--      the package can be used and modified successfully in the appending and 
--      retrieval of data.
--      
-- TEST DESCRIPTION:
--      This test is designed to simulate an environment where a data file
--      that holds floating point information is created, written to, and 
--      closed.  In the future, the file can be reopened in Append_File mode,
--      additional data can be appended to it, and then closed.  This process
--      of Open/Append/Close can be repeated as necessary.  All data written
--      to the file is verified for accuracy when retrieved from the file.
--      
--      This test verifies issues of create in Append_File mode, appending to
--      a file previously appended to, opening in Append_File mode, resetting
--      from Append_File mode to In_File mode, as well as a variety of Text_IO
--      and Float_IO predefined subprograms.
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

procedure CXAA009 is

   use Ada;
   Loan_File : Text_IO.File_Type;
   Loan_Filename : constant String :=
                              Report.Legal_File_Name ( Nam => "CXAA009" );
   Incomplete : exception;

begin

   Report.Test ("CXAA009", "Check that the capabilities of "               &
                           "Text_IO.Float_IO operate correctly for files " &
                           "with mode Append_File");

   Test_for_Text_IO_Support:
   begin

      -- An implementation that does not support Text_IO in a particular
      -- environment will raise Use_Error on calls to various
      -- Text_IO operations.  This block statement encloses a call to
      -- Create, which should raise the exception in a non-supportive 
      -- environment.  This exception will be handled to produce a
      -- Not_Applicable result.

      Text_IO.Create (File => Loan_File,                    -- Create in
                      Mode => Text_IO.Out_File,             -- Out_File mode.
                      Name => Loan_Filename);

   exception

       when Text_IO.Use_Error | Text_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Text_IO" );
          raise Incomplete;

   end Test_for_Text_IO_Support;

   Operational_Test_Block:
   declare
      Total_Loans_Outstanding : constant Natural := 3;
      Transaction_Status      :          Boolean := False;

      type Account_Balance_Type is digits 6 range 0.0 .. 1.0E6;
      type Loan_Balance_Type    is digits 6; 
      type Interest_Rate_Type   is digits 4 range 0.0 .. 30.00;

      type Loan_Info_Type is record
         Account_Balance    : Account_Balance_Type := 0.00;
         Loan_Balance       : Loan_Balance_Type    := 0.00;
         Loan_Interest_Rate : Interest_Rate_Type   := 0.00;
      end record;   

      Home_Refinance_Loan : Loan_Info_Type := 
                              (14_500.00, 135_000.00, 6.875);
      Line_Of_Credit_Loan : Loan_Info_Type := 
                              (  5490.00,   -3000.00, 13.75);
      Small_Business_Loan : Loan_Info_Type := 
                              (Account_Balance    => 45_000.00,
                               Loan_Balance       => 10_500.00,
                               Loan_Interest_Rate =>     5.875);

      package Acct_IO is new Text_IO.Float_IO (Account_Balance_Type);
      package Loan_IO is new Text_IO.Float_IO (Loan_Balance_Type);
      package Rate_IO is new Text_IO.Float_IO (Interest_Rate_Type);


      -- The following procedure performs the addition of loan information
      -- into a data file.  Boolean status of True is returned if all of
      -- the data entry was successful, False otherwise.
      -- This demonstrates use of Float_IO using a variety of data formats.

      procedure Update_Loan_Info (The_File : in out Text_IO.File_Type;
                                  The_Loan : in     Loan_Info_Type;
                                  Status   :    out Boolean ) is
      begin
         Acct_IO.Put (The_File, The_Loan.Account_Balance);    
         Loan_IO.Put (The_File, The_Loan.Loan_Balance, 15, 2, 0);
         Rate_IO.Put (File => The_File, 
                      Item => The_Loan.Loan_Interest_Rate,
                      Fore => 6,
                      Aft  => 3,
                      Exp  => 0);
         Text_IO.New_Line (The_File);
         Status := True;
      exception
         when others => Status := False;
      end Update_Loan_Info;


   begin

      -- This code section simulates a bank maintaining a data file 
      -- containing information on loans that have been made.
      -- The scenario:
      --    The loan file was created in Out_File mode. 
      --    Some number of data records are added.
      --    The file is closed.
      --    The file is subsequently reopened in Append_File mode.
      --    Data is appended to the file.
      --    The file is closed.
      --    Repeat the Open/Append/Close process as required.
      --    Verify data in the file.
      --    etc.

      Update_Loan_Info(Loan_File, Home_Refinance_Loan, Transaction_Status);

      if not Transaction_Status then
         Report.Failed ("Failure in update of first loan data");
      end if;

      Text_IO.Close (Loan_File);

      -- When subsequent data items are to be added to the file, the file
      -- is opened in Append_File mode.

      Text_IO.Open (Loan_File,                            -- Open with
                    Text_IO.Append_File,                  -- Append mode.
                    Loan_Filename);
                                                             
      Update_Loan_Info(Loan_File, Line_Of_Credit_Loan, Transaction_Status);

      if not Transaction_Status then
         Report.Failed("Failure in update of first loan data");
      end if;

      Text_IO.Close(Loan_File);

      -- To add additional data to the file, the file
      -- is again opened in Append_File mode (appending to a file
      -- previously appended to).

      Text_IO.Open (Loan_File,                            -- Open with
                    Text_IO.Append_File,                  -- Append mode.
                    Loan_Filename);
                                                             
      Update_Loan_Info(Loan_File, Small_Business_Loan, Transaction_Status);

      if not Transaction_Status then
         Report.Failed("Failure in update of first loan data");
      end if;

      Test_Verification_Block:                             
      declare
         type Ledger_Type is 
           array (1 .. Total_Loans_Outstanding) of Loan_Info_Type;
         TC_Bank_Ledger : Ledger_Type;
         TC_Item_Count  : Natural := 0;
      begin                                                

         Reset1:
         begin
            Text_IO.Reset (Loan_File, Text_IO.In_File);       -- Reset for
                                                              -- reading.
         exception
            when Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Text_IO" );
               raise Incomplete;
         end Reset1;

         while not Text_IO.End_Of_File (Loan_File) loop
            TC_Item_Count := TC_Item_Count + 1;               
            Acct_IO.Get (Loan_File, 
                         TC_Bank_Ledger(TC_Item_Count).Account_Balance);    
            Loan_IO.Get (Loan_File, 
                         TC_Bank_Ledger(TC_Item_Count).Loan_Balance, 
                         0);
            Rate_IO.Get(File  => Loan_File, 
                        Item  => 
                          TC_Bank_Ledger(TC_Item_Count).Loan_Interest_Rate,
                        Width => 0);
            Text_IO.Skip_Line(Loan_File);

         end loop;

         -- Verify all of the data fields read from the file.  Compare
         -- with the values that were originally entered into the file.

         if (TC_Bank_Ledger(1) /= Home_Refinance_Loan) or
            (TC_Bank_Ledger(2) /= Line_Of_Credit_Loan) or
            (TC_Bank_Ledger(3) /= Small_Business_Loan) then
            Report.Failed("Error in data read from file");
         end if;

         if (TC_Item_Count /= Total_Loans_Outstanding) then
            Report.Failed ("Incorrect number of records read from file");
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
            Report.Failed ("Exception in Text_IO.Float_IO processing");
   end Operational_Test_Block;

   Final_Block:
   begin
      -- Delete the external file.
      if Text_IO.Is_Open(Loan_File) then
         Text_IO.Delete(Loan_File);
      else
         Text_IO.Open(Loan_File, Text_IO.In_File, Loan_Filename);
         Text_IO.Delete(Loan_File);
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

end CXAA009;
