-- CXAC001.A
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
--      Check that the attribute T'Write will, for any specific non-limited
--      type T, write an item of the subtype to the stream.
--
--      Check that the attribute T'Read will, for a specific non-limited
--      type T, read a value of the subtype from the stream.
--
-- TEST DESCRIPTION:
--      The scenario depicted in this test is that of an environment where 
--      product data is stored in stream form, then reconstructed into the
--      appropriate data structures.  Several records of product information
--      are stored in an array; the array is passed as a parameter to a 
--      procedure for storage in the stream.  A header is created based on the
--      number of data records stored in the array.  The header is then written
--      to the stream, followed by each record maintained in the array.
--      In order to retrieve data from the stream, the header information is
--      read from the stream, and the data stored in the header is used to 
--      perform the appropriate number of read operations of record data from
--      the stream.  All data read from the stream is validated against the
---     values that were written to the stream.
--
-- APPLICABILITY CRITERIA: 
--      Applicable to all systems capable of supporting IO operations on 
--      external Stream_IO files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      08 Nov 95   SAIC    Corrected call to Read in Procedure Retrieve_Data
--                          for ACVC 2.0.1.
--      27 Feb 08   PWB.CTA Allowed for non-support of certain IO operations.
--!

with Ada.Streams.Stream_IO;
with Report;

procedure CXAC001 is

   package Strm_Pack renames Ada.Streams.Stream_IO;
   The_File     : Strm_Pack.File_Type;
   The_Filename : constant String :=
                     Report.Legal_File_Name ( Nam => "CXAC001" );
   Incomplete : exception;


begin 

   Report.Test ("CXAC001", "Check that the 'Read and 'Write attributes " &
                           "will transfer an object of a specific, "     &
                           "non-limited type to/from a stream");

   Test_for_Stream_IO_Support:
   begin

      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on 
      -- calls to various Stream_IO operations.  This block statement 
      -- encloses a call to Create, which should produce an exception in a 
      -- non-supportive environment.  These exceptions will be handled to 
      -- produce a Not_Applicable result.

      Strm_Pack.Create (The_File, Strm_Pack.Out_File, The_Filename);

   exception

       when Ada.Streams.Stream_IO.Use_Error | 
            Ada.Streams.Stream_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Stream_IO" );
          raise Incomplete;

   end Test_for_Stream_IO_Support;

   Operational_Test_Block:
   declare

      The_Stream : Strm_Pack.Stream_Access;
      Todays_Date : String (1 .. 6) := "271193";

      type ID_Type   is range 1 .. 100;
      type Size_Type is (Small, Medium, Large, XLarge);

      type Header_Type is record
         Number_of_Elements : Natural := 0;
         Origination_Date   : String (1 .. 6);
      end record;

      type Data_Type is record
         ID   : ID_Type;
         Size : Size_Type;
      end record;

      type Data_Array_Type is array (Positive range <>) of Data_Type;

      Product_Information_1 : Data_Array_Type (1 .. 3) := ((20, Large),
                                                           (55, Small),
                                                           (89, XLarge));

      Product_Information_2 : Data_Array_Type (1 .. 4) := (( 5, XLarge),
                                                           (27, Small),
                                                           (79, Medium),
                                                           (93, XLarge));

      procedure Store_Data ( The_Stream : in Strm_Pack.Stream_Access;
                             The_Array  : in Data_Array_Type ) is
         Header     : Header_Type;
      begin

         -- Fill in header info.
         Header.Number_of_Elements := The_Array'Length;
         Header.Origination_Date := Todays_Date;       

         -- Write header to stream.
         Header_Type'Write (The_Stream, Header);   
                                                      
         -- Write each record in the array to the stream.
         for I in 1 .. Header.Number_of_Elements loop
           Data_Type'Write (The_Stream, The_Array (I));
         end loop;                                     

      end Store_Data;

      procedure Retrieve_Data (The_Stream : in     Strm_Pack.Stream_Access;
                               The_Header :    out Header_Type;
                               The_Array  :    out Data_Array_Type ) is
      begin

         -- Read header from the stream.
         Header_Type'Read (The_Stream, The_Header);  
                                                   
         -- Read the records from the stream into the array.
         for I in 1 .. The_Header.Number_of_Elements loop    
            Data_Type'Read (The_Stream, The_Array (I));  
         end loop;                                       

      end Retrieve_Data;

   begin

      -- Assign access value.
      The_Stream := Strm_Pack.Stream (The_File);      

      -- Product information is to be stored in the stream file.  These
      -- data arrays are of different sizes (actually, the records
      -- are stored individually, not as a single array).  Prior to the
      -- record data being written, a header record is initialized with
      -- information about the data to be written, then itself is written
      -- to the stream.

      Store_Data (The_Stream, Product_Information_1);
      Store_Data (The_Stream, Product_Information_2);

      Test_Verification_Block:
      declare
         Product_Header_1 : Header_Type;
         Product_Header_2 : Header_Type;
         Product_Array_1  : Data_Array_Type (1 .. 3);
         Product_Array_2  : Data_Array_Type (1 .. 4);
      begin

         Reset1:
         begin
            Strm_Pack.Reset (The_File, Strm_Pack.In_File);
         exception
            when Ada.Streams.Stream_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Stream_IO" );
               raise Incomplete;
         end Reset1;

         -- Data is read from the stream, first the appropriate header, 
         -- then the associated data records, which are then reconstructed
         -- into a data array of product information.

         Retrieve_Data (The_Stream, Product_Header_1, Product_Array_1);

         -- Validate a field in the header.
         if (Product_Header_1.Origination_Date   /= Todays_Date) or
            (Product_Header_1.Number_of_Elements /= 3)    
         then
            Report.Failed ("Incorrect Header_1 info read from stream");
         end if;

         -- Validate the data records read from the file.
         for I in 1 .. Product_Header_1.Number_of_Elements loop
            if (Product_Array_1(I) /= Product_Information_1(I)) then
               Report.Failed ("Incorrect Product 1 info read from" &
                              " record: " & Integer'Image (I));
            end if;
         end loop;

         -- Repeat this read and verify operation for the next parcel of
         -- data.  Again, header and data record information are read from
         -- the same stream file.  
         Retrieve_Data (The_Stream, Product_Header_2, Product_Array_2);

         if (Product_Header_2.Origination_Date   /= Todays_Date) or
            (Product_Header_2.Number_of_Elements /= 4)    
         then 
            Report.Failed ("Incorrect Header_2 info read from stream");
         end if;

         for I in 1 .. Product_Header_2.Number_of_Elements loop
            if (Product_Array_2(I) /= Product_Information_2(I)) then
               Report.Failed ("Incorrect Product_2 info read from" &
                              " record: " & Integer'Image (I));
            end if;
         end loop;

      exception

         when Incomplete =>
            raise; 

         when Strm_Pack.End_Error =>           -- If correct number of 
                                               -- items not in file (data 
                                               -- overwritten), then fail.
            Report.Failed ("Incorrect number of record elements in file");
            if not Strm_Pack.Is_Open (The_File) then
               Strm_Pack.Open (The_File, Strm_Pack.Out_File, The_Filename);
            end if;

         when others => 
            Report.Failed ("Exception raised in Data Verification Block");
            if not Strm_Pack.Is_Open (The_File) then
               Strm_Pack.Open (The_File, Strm_Pack.Out_File, The_Filename);
            end if;

      end Test_Verification_Block;

   exception

      when Incomplete =>
         raise; 

      when others => 
         Report.Failed ("Exception raised in Operational Test Block");

   end Operational_Test_Block;

   Deletion:
   begin
      -- Delete the file.
      if Strm_Pack.Is_Open (The_File) then
         Strm_Pack.Delete (The_File);
      else
         Strm_Pack.Open (The_File, Strm_Pack.Out_File, The_Filename);
         Strm_Pack.Delete (The_File);
      end if;

   exception

      when others =>
         Report.Failed
            ( "Delete not properly implemented for Stream_IO" );
   end Deletion;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAC001;
