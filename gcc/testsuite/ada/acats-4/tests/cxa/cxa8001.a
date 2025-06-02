-- CXA8001.A
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
--      Check that all elements to be transferred to a sequential file of
--      mode Append_File will be placed following the last element currently
--      in the file.  
--      Check that it is possible to append data to a file that has been 
--      previously appended to.
--      Check that the predefined procedure Write will place an element after
--      the last element in the file in mode Append_File.
--
-- TEST DESCRIPTION:
--      This test implements a sequential file system that has the capability
--      to store data records at the end of a file.  Initially, the file is 
--      opened with mode Out_File, and data is written to the file.  The file
--      is closed, then reopened with mode Append_File.  An additional record
--      is written, and again the file is closed.  The file is then reopened,
--      again with mode Append_File, and another record is written to the 
--      file.
--      The file is closed again, the reopened with mode In_File, and the data
--      in the file is read and checked for proper ordering within the file.
--
--      An expected common usage of Append_File mode would be in the opening
--      of a file that currently contains data.  Likewise, the reopening of
--      files in Append_Mode that have been previously appended to for the
--      addition of more data would be frequently encountered.  This test
--      attempts to simulate both situations.  (Of course, in an actual user
--      environment, the open/write/close processing would be performed using
--      looping structures, rather than the straight-line processing displayed
--      here.)
--
-- APPLICABILITY CRITERIA: 
--      Applicable to all systems capable of supporting IO operations on 
--      external Sequential_IO files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      27 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--!

with Sequential_IO;
with Report;

procedure CXA8001 is

   -- Declare data types and objects to be stored in the file.
   subtype Name_Type is String (1 .. 10);
   type    Tickets   is range 0 .. 1000;

   type Order_Type is record
      Name          : Name_Type;
      No_of_Tickets : Tickets;
   end record;
   
   package Order_IO is new Sequential_IO (Order_Type); -- Declare Seq_IO 
                                                       -- package,
   Order_File     : Order_IO.File_Type;                -- and file object.
   Order_Filename : constant String := 
                           Report.Legal_File_Name ( Nam => "CXA8001" );
   Incomplete : exception;

begin

   Report.Test ("CXA8001", "Check that all elements to be transferred to a " &
                           "sequential file of mode Append_File will be "    &
                           "placed following the last element currently "    &
                           "in the file");

   Test_for_Sequential_IO_Support:
   begin

      -- An implementation that does not support Sequential_IO in a particular
      -- environment will raise Use_Error or Name_Error on calls to various
      -- Sequential_IO operations.  This block statement encloses a call to
      -- Create, which should produce an exception in a non-supportive 
      -- environment.  These exceptions will be handled to produce a
      -- Not_Applicable result.

      Order_IO.Create (File => Order_File,        -- Create Sequential_IO file 
                       Mode => Order_IO.Out_File, -- with mode Out_File.
                       Name => Order_Filename);

   exception

       when Order_IO.Use_Error | Order_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Sequential_IO" );
          raise Incomplete;

   end Test_for_Sequential_IO_Support;

   Operational_Test_Block:
   declare
      -- Assign values into the component fields of the data objects.
      Buyer_1 : constant Order_Type := ("John Smith", 3);
      Buyer_2 : constant Order_Type := 
                  (Name => "Jane Jones", No_of_Tickets => 2);
      Buyer_3 : Order_Type := ("Mike Brown", 5);

   begin
      Order_IO.Write (File => Order_File,      -- Write initial data item 
                      Item => Buyer_1);        -- to file.

      Order_IO.Close (File => Order_File);     -- Close file.

      --
      -- Enter additional data records into the file.  (Append to a file of
      -- previous mode Out_File).
      --
      Order_IO.Open (Order_File,               -- Open Sequential_IO file 
                     Order_IO.Append_File,     -- with mode Append_File.
                     Order_Filename);
                                               
      Order_IO.Write (Order_File, Buyer_2);    -- Write second data item 
                                               -- to file.
      Order_IO.Close (File => Order_File);     -- Close file.

      -- Check to determine whether file is actually closed.
      begin
         Order_IO.Write (Order_File, Buyer_2);   
         Report.Failed("Exception not raised on Write to Closed file");
      exception
         when Order_IO.Status_Error => null;   -- Expected exception.
         when others                => 
           Report.Failed("Incorrect exception on Write to Closed file");
      end;

      --
      -- The following code segment demonstrates appending data to a file 
      -- that has been previously appended to.
      --

      Order_IO.Open (Order_File,               -- Open Sequential_IO file 
                     Order_IO.Append_File,     -- with mode Append_File.
                     Order_Filename );

      Order_IO.Write (Order_File, Buyer_3);    -- Write third data item 
                                               -- to file.
      Order_IO.Close (File => Order_File);     -- Close file.


      Test_Verification_Block:
      declare
         TC_Order1, TC_Order2, TC_Order3 : Order_Type;
      begin
      
         Order_IO.Open (Order_File,            -- Open Sequential_IO file 
                        Order_IO.In_File,      -- with mode In_File.
                        Order_Filename );

         Order_IO.Read (File => Order_File,    -- Read records from file.
                        Item => TC_Order1);       
         Order_IO.Read (Order_File, TC_Order2);   
         Order_IO.Read (Order_File, TC_Order3);   

         -- Compare the contents of each with the individual data items.
         -- If items read from file do not match the items placed into 
         -- the file, in the appropriate order, then fail.

         if ((TC_Order1 /= Buyer_1)               or
             (TC_Order2.Name /= Buyer_2.Name)     or
             (TC_Order2.No_of_Tickets /= Buyer_2.No_of_Tickets) or
             not ((TC_Order3.Name = "Mike Brown") and
                  (TC_Order3.No_of_Tickets = 5))) then
            Report.Failed ("Incorrect appending of record data in file");
         end if;

         -- Check to determine that no more than three data records were
         -- actually written to the file.
         if not Order_IO.End_Of_File (Order_File) then
            Report.Failed("File not empty after three reads");
         end if;

      exception
      
         when Order_IO.End_Error =>            -- If three items not in 
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

   Deletion:
   begin
      -- Check that file is open prior to deleting it.
      if Order_IO.Is_Open(Order_File) then
         Order_IO.Delete (Order_File);         
      else
         Order_IO.Open(Order_File, Order_IO.In_File, Order_Filename);
         Order_IO.Delete (Order_File);          
      end if;

   exception
      when others =>
         Report.Failed
            ( "Delete not properly implemented for Sequential_IO" );

   end Deletion;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXA8001;
