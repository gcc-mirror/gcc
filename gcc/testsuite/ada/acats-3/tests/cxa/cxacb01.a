-- CXACB01.A
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
--      Check that the default attributes 'Input and 'Output work properly when 
--      used with objects of a variety of types, including two-dimensional 
--      arrays and records without default discriminants.
--
-- TEST DESCRIPTION:
--      This test simulates utility company service record storage, using 
--      Stream_IO to allow the storage of heterogeneous data in a single 
--      stream file.
--      
--      Three types of data are written to the stream file for each utility
--      service customer.
--      First, the general information on the customer is written.  
--      This is an object of a discriminated (without default) record
--      type.  This is followed by an integer object containing a count of
--      the number of service months for the customer. Finally, a
--      two-dimensional array object with monthly consumption information for 
--      the customer is written to the stream.
--      
--      Objects of record types with discriminants without defaults should
--      have their discriminants included in the stream when using 'Output. 
--      Likewise, discriminants should be extracted 
--      from the stream when using 'Input. Similarly, array bounds are written
--      to and read from the stream when using 'Output and 'Input with array
--      objects.
--
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations that support external
--      Stream_IO files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with FXACB00;
with Ada.Streams.Stream_IO;
with Report;

procedure CXACB01 is
begin

   Report.Test ("CXACB01", "Check that the default attributes 'Input and " &
                           "'Output work properly when used with objects " &
                           "of record, natural, and array types" );

   Test_for_Stream_IO_Support:
   declare

      Util_File                : Ada.Streams.Stream_IO.File_Type;
      Util_Stream              : Ada.Streams.Stream_IO.Stream_Access;
      Utility_Service_Filename : constant String := Report.Legal_File_Name;

   begin

      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on 
      -- calls to various Stream_IO operations.  This block statement 
      -- encloses a call to Create, which should produce an exception in a 
      -- non-supportive environment.  These exceptions will be handled to 
      -- produce a Not_Applicable result.

      Ada.Streams.Stream_IO.Create (Util_File, 
                                    Ada.Streams.Stream_IO.Out_File,
                                    Utility_Service_Filename);

      Operational_Test_Block:
      declare

         -- The following procedure will store all of the customer specific
         -- information into the stream.

         procedure Store_Data_In_Stream
           (Customer : in FXACB00.Service_Type;
            Months   : in FXACB00.Months_In_Service_Type;
            History  : in FXACB00.Service_History_Type) is
         begin
            FXACB00.Service_Type'Output           (Util_Stream, Customer);
            FXACB00.Months_In_Service_Type'Output (Util_Stream, Months);
            FXACB00.Service_History_Type'Output   (Util_Stream, History);
         end Store_Data_In_Stream;


         -- The following procedure will remove from the stream all of the 
         -- customer related information.

         procedure Retrieve_Data_From_Stream
           (Customer : out FXACB00.Service_Type;
            Months   : out FXACB00.Months_In_Service_Type;
            History  : out FXACB00.Service_History_Type) is
         begin
            Customer := FXACB00.Service_Type'Input           (Util_Stream);
            Months   := FXACB00.Months_In_Service_Type'Input (Util_Stream);
            History  := FXACB00.Service_History_Type'Input   (Util_Stream);
         end Retrieve_Data_From_Stream;


      begin

         Util_Stream := Ada.Streams.Stream_IO.Stream (Util_File);

         -- Write all of the customer service information (record, numeric,
         -- and array objects) defined in package FXACB00 into the stream.

         Data_Storage_Block:
         begin

            Store_Data_In_Stream (Customer => FXACB00.Customer1,
                                  Months   => FXACB00.C1_Months,
                                  History  => FXACB00.C1_Service_History);

            Store_Data_In_Stream (FXACB00.Customer2, 
                                  FXACB00.C2_Months, 
                                  History  => FXACB00.C2_Service_History);

            Store_Data_In_Stream (Months   => FXACB00.C3_Months,
                                  History  => FXACB00.C3_Service_History,
                                  Customer => FXACB00.Customer3);
         end Data_Storage_Block;


         Data_Verification_Block:
         declare

            TC_Residence  : FXACB00.Service_Type (FXACB00.Residence);
            TC_Apartment  : FXACB00.Service_Type (FXACB00.Apartment);
            TC_Commercial : FXACB00.Service_Type (FXACB00.Commercial);


            TC_Months1, 
            TC_Months2, 
            TC_Months3    : FXACB00.Months_In_Service_Type :=
                              FXACB00.Months_In_Service_Type'First; 


            TC_History1   : 
              FXACB00.Service_History_Type (FXACB00.Quarterly_Period_Type,
                                            FXACB00.Month_In_Quarter_Type) :=
              (others => (others => FXACB00.Electric_Usage_Type'Last));

            TC_History2   :
              FXACB00.Service_History_Type 
                (FXACB00.Quarterly_Period_Type range 
                   FXACB00.Spring .. FXACB00.Summer,
                 FXACB00.Month_In_Quarter_Type) :=
              (others => (others => FXACB00.Electric_Usage_Type'Last));

            TC_History3   :
              FXACB00.Service_History_Type (FXACB00.Quarterly_Period_Type,
                                            FXACB00.Month_In_Quarter_Type) :=
              (others => (others => FXACB00.Electric_Usage_Type'Last));

         begin

            Ada.Streams.Stream_IO.Reset (Util_File, 
                                         Ada.Streams.Stream_IO.In_File);

            -- Input all of the data that is contained in the stream.
            -- Compare all data with the original data in package FXACB00 
            -- that was written to the stream.

            Retrieve_Data_From_Stream (TC_Residence, TC_Months1, TC_History1);
            Retrieve_Data_From_Stream (TC_Apartment, TC_Months2, TC_History2);
            Retrieve_Data_From_Stream (Customer => TC_Commercial, 
                                       Months   => TC_Months3, 
                                       History  => TC_History3);

            -- After all the data has been correctly extracted, the file 
            -- should be empty.

            if not Ada.Streams.Stream_IO.End_Of_File (Util_File) then
               Report.Failed ("Stream file not empty");
            end if;

            -- Verify that the data values read from the stream are the same
            -- as those written to the stream.

            if ((FXACB00."/="(FXACB00.Customer1, TC_Residence)) or else
                (FXACB00."/="(FXACB00.Customer2, TC_Apartment)) or else
                (FXACB00."/="(FXACB00.Customer3, TC_Commercial)))
            then
               Report.Failed ("Customer information incorrect");
            end if;

            if ((FXACB00."/="(FXACB00.C1_Months, TC_Months1)) or
                (FXACB00."/="(FXACB00.C2_Months, TC_Months2)) or
                (FXACB00."/="(FXACB00.C3_Months, TC_Months3))) 
            then
               Report.Failed ("Number of Months information incorrect");
            end if;

            if not ((FXACB00."="(FXACB00.C1_Service_History, TC_History1)) and
                    (FXACB00."="(FXACB00.C2_Service_History, TC_History2)) and 
                    (FXACB00."="(FXACB00.C3_Service_History, TC_History3)))
            then
               Report.Failed ("Service history information incorrect");
            end if;

         end Data_Verification_Block;

      exception

         when others => 
            Report.Failed ("Exception raised in Operational Test Block");

      end Operational_Test_Block;

      -- Delete the file.
      if Ada.Streams.Stream_IO.Is_Open (Util_File) then
         Ada.Streams.Stream_IO.Delete (Util_File);
      else
         Ada.Streams.Stream_IO.Open (Util_File,
                                     Ada.Streams.Stream_IO.Out_File, 
                                     Utility_Service_Filename);
         Ada.Streams.Stream_IO.Delete (Util_File);
      end if;


   exception

      -- Since Use_Error or Name_Error can be raised if, for the specified
      -- mode, the environment does not support Stream_IO operations,
      -- the following handlers are included:

      when Ada.Streams.Stream_IO.Name_Error =>
         Report.Not_Applicable ("Name_Error raised on Stream IO Create");

      when Ada.Streams.Stream_IO.Use_Error  =>
         Report.Not_Applicable ("Use_Error raised on Stream IO Create");

      when others                           =>
         Report.Failed ("Unexpected exception raised");

   end Test_for_Stream_IO_Support;

   Report.Result;

end CXACB01;
