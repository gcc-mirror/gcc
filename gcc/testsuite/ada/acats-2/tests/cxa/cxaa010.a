-- CXAA010.A
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
--      Check that the operations defined in package Ada.Text_IO.Decimal_IO
--      are available, and that they function correctly when used for the
--      input/output of Decimal types.
--
-- TEST DESCRIPTION:
--      This test demonstrates the Put and Get procedures found in the
--      generic package Ada.Text_IO.Decimal_IO.  Both Put and Get are
--      overloaded to allow placement or extraction of decimal values 
--      to/from a text file or a string.  This test demonstrates both forms
--      of each subprogram.  
--      The test defines an array of records containing decimal value
--      and string component fields.  All component values are placed in a 
--      Text_IO file, with the decimal values being placed there using the 
--      version of Put defined for files, and using user-specified formatting 
--      parameters.  The data is later extracted from the file, with the 
--      decimal values being removed using the version of Get defined for
--      files.  Decimal values are then written to strings, using the 
--      appropriate Put procedure.  Finally, extraction of the decimal data
--      from the strings completes the evaluation of the Decimal_IO package
--      subprograms.
--      The reconstructed data is verified at the end of the test against the
--      data originally written to the file.
--
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations capable of supporting external
--      Text_IO files and Decimal Fixed Point Types
-- 
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Information Systems Annex (F):
--        this test must execute and report PASSED.
--
--      For implementations not validating against Annex F:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-F RQMT", in which case it may be graded as inapplicable.
--        Otherwise, the test must execute and report PASSED.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      20 Feb 95   SAIC    Modified test to allow for Use_Error/Name_Error
--                          generation by an implementation not supporting
--                          Text_IO operations.
--      14 Nov 95   SAIC    Corrected string indexing for ACVC 2.0.1.
--      27 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--      16 FEB 98   EDS     Modified documentation.
--!

with Ada.Text_IO;
with Report;

procedure CXAA010 is
   use Ada.Text_IO;
   Tax_Roll      : Ada.Text_IO.File_Type;
   Tax_Roll_Name : constant String :=
                           Report.Legal_File_Name ( Nam => "CXAA010" );
   Incomplete : exception;
begin

   Report.Test ("CXAA010", "Check that the operations defined in package " &
                           "Ada.Text_IO.Decimal_IO are available, and "    &
                           "that they function correctly when used for "   &
                           "the input/output of Decimal types");

   Test_for_Decimal_IO_Support:
   begin

      -- An implementation that does not support Text_IO creation or naming
      -- of external files in a particular environment will raise Use_Error 
      -- or Name_Error on a call to Text_IO Create. This block statement 
      -- encloses a call to Create, which should produce an exception in a 
      -- non-supportive environment.  Either of these exceptions will be 
      -- handled to produce a Not_Applicable result.

      Ada.Text_IO.Create (Tax_Roll, Ada.Text_IO.Out_File, Tax_Roll_Name);

   exception

       when Ada.Text_IO.Use_Error | Ada.Text_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Text_IO" );
          raise Incomplete;

   end Test_for_Decimal_IO_Support;

   Taxation:
   declare

      ID_Length           : constant :=  5;
      Price_String_Length : constant :=  5;
      Value_String_Length : constant :=  6;
      Total_String_Length : constant := 20;
      Spacer              : constant String := "  ";  -- Two blanks.

      type Price_Type     is delta 0.1  digits 4;              -- ANX-F RQMT
      type Value_Type     is delta 0.01 digits 5;              -- ANX-F RQMT

      type Property_Type  is 
         record
            Parcel_ID      : String (1..ID_Length);
            Purchase_Price : Price_Type;
            Assessed_Value : Value_Type;
         end record;
     
      type    City_Block_Type     is array (1..4) of Property_Type;

      subtype Tax_Bill_Type       is string (1..Total_String_Length);
      type    Tax_Bill_Array_Type is array (1..4) of Tax_Bill_Type;

      Neighborhood : City_Block_Type := 
        (("X9254", 123.0, 135.00), ("X3569", 345.0, 140.50),
         ("X3434", 234.0, 179.50), ("X8838", 456.0, 158.00));
      
      Neighborhood_Taxes : Tax_Bill_Array_Type;

      package Price_IO is new Ada.Text_IO.Decimal_IO (Price_Type);
      package Value_IO is new Ada.Text_IO.Decimal_IO (Value_Type);

   begin  -- Taxation

      Assessors_Office:
      begin

         for Parcel in City_Block_Type'Range loop
            -- Note: All data in the file will be separated with a 
            --       two-character blank spacer.
            Ada.Text_IO.Put(Tax_Roll, Neighborhood(Parcel).Parcel_ID);
            Ada.Text_IO.Put(Tax_Roll, Spacer);

            -- Use Decimal_IO.Put with non-default format parameters to
            -- place decimal data into file.
            Price_IO.Put   (Tax_Roll, Neighborhood(Parcel).Purchase_Price,
                            Fore => 3, Aft =>1, Exp => 0);
            Ada.Text_IO.Put(Tax_Roll, Spacer);

            Value_IO.Put   (Tax_Roll, Neighborhood(Parcel).Assessed_Value,
                            Fore => 3, Aft =>2, Exp => 0);
            Ada.Text_IO.New_Line(Tax_Roll);
         end loop;

         Ada.Text_IO.Close (Tax_Roll);

      exception
         when others => 
            Report.Failed ("Exception raised in Assessor's Office");
      end Assessors_Office;


      Twice_A_Year:
      declare

         procedure Collect_Tax(Index     : in     Integer;
                               Tax_Array : in out Tax_Bill_Array_Type) is
            ID            : String (1..ID_Length);
            Price         : Price_Type := 0.0; 
            Value         : Value_Type := 0.00;
            Price_String  : String (1..Price_String_Length);
            Value_String  : String (1..Value_String_Length);
         begin  

            -- Extract information from the Text_IO file; one string, two
            -- decimal values.
            -- Note that the Spacers that were put in the file above are
            -- not individually read here, due to the fact that each call
            -- to Decimal_IO.Get below uses a zero in the Width field,
            -- which allows each Get procedure to skip these leading blanks
            -- prior to extracting the numeric value.
               
            Ada.Text_IO.Get (Tax_Roll, ID);

            -- A zero value of Width is provided, so the following
            -- two calls to Decimal_IO.Get will skip the leading blanks,
            -- (from the Spacer variable above), then read the numeric
            -- literals.

            Price_IO.Get    (Tax_Roll, Price, 0);
            Value_IO.Get    (Tax_Roll, Value, 0);
            Ada.Text_IO.Skip_Line (Tax_Roll);

            -- Convert the values read from the file into string format,
            -- using user-specified format parameters.
            -- Format of the Price_String should be "nnn.n"
            -- Format of the Value_String should be "nnn.nn"

            Price_IO.Put (To   => Price_String,
                          Item => Price,
                          Aft  => 1);
            Value_IO.Put (Value_String, Value, 2);

            -- Construct a string of length 20 that contains the Parcel_ID,
            -- the Purchase_Price, and the Assessed_Value, separated by
            -- two-character blank data spacers.  Store this string
            -- into the string array out parameter.
            -- Format of each Tax_Array element should be 
            -- "Xnnnn  nnn.n  nnn.nn" (with an 'n' signifying a digit).

            Tax_Array(Index) := ID           & Spacer &
                                Price_String & Spacer &
                                Value_String;            
         exception
            when Data_Error =>
               Report.Failed("Data Error raised during the extraction " &
                             "of decimal data from the file");
            when others     => 
              Report.Failed("Exception in Collect_Tax procedure");
         end Collect_Tax;

  
      begin  -- Twice_A_Year
      
         Ada.Text_IO.Open (Tax_Roll, Ada.Text_IO.In_File, Tax_Roll_Name);

         -- Determine property tax bills for the entire neighborhood from
         -- the information that is stored in the file. Store information
         -- in the Neighborhood_Taxes string array.

         for Parcel in City_Block_Type'Range loop
            Collect_Tax (Parcel, Neighborhood_Taxes);
         end loop;

      exception
         when others => 
           Report.Failed ("Exception in Twice_A_Year Block");
      end Twice_A_Year;

      -- Use Decimal_IO Get procedure to extract information from a string.
      -- Verify data against original values.
      Validation_Block:
      declare
         TC_ID     : String (1..ID_Length);    -- 1..5
         TC_Price  : Price_Type;
         TC_Value  : Value_Type;
         Length    : Positive;
         Front,
         Rear      : Integer := 0;
      begin

         for Parcel in City_Block_Type'Range loop
            -- Extract values from the strings of the string array.
            -- Each element of the string array is 20 characters long; the
            -- first five characters are the Parcel_ID, two blank characters
            -- separate data, the next five characters contain the Price 
            -- decimal value, two blank characters separate data, the last 
            -- six characters contain the Value decimal value.  
            -- Extract each of these components in turn.

            Front := 1;                                        --  1
            Rear  := ID_Length;                                --  5
            TC_ID := Neighborhood_Taxes(Parcel)(Front..Rear);

            -- Extract the decimal value from the next slice of the string.
            Front := Rear + 3;                                 --  8
            Rear  := Front + Price_String_Length - 1;          -- 12
            Price_IO.Get (Neighborhood_Taxes(Parcel)(Front..Rear),
                          Item => TC_Price,
                          Last => Length);

            -- Extract next decimal value from slice of string, based on 
            -- length of preceding strings read from string array element.
            Front := Rear + 3;                                 -- 15  
            Rear  := Total_String_Length;                      -- 20
            Value_IO.Get (Neighborhood_Taxes(Parcel)(Front..Rear),
                          Item => TC_Value,
                          Last => Length);

            if TC_ID    /= Neighborhood(Parcel).Parcel_ID       or
               TC_Price /= Neighborhood(Parcel).Purchase_Price  or
               TC_Value /= Neighborhood(Parcel).Assessed_Value  
            then
               Report.Failed ("Incorrect data validation");
            end if;

         end loop;

      exception
         when others => Report.Failed ("Exception in Validation Block");
      end Validation_Block;

      -- Check that the Text_IO file is open, then delete.

      if not Ada.Text_IO.Is_Open (Tax_Roll) then
         Report.Failed ("File not left open after processing");
         Ada.Text_IO.Open (Tax_Roll, Ada.Text_IO.Out_File, Tax_Roll_Name);
      end if;

      Ada.Text_IO.Delete (Tax_Roll);

   exception
      when others => 
         Report.Failed ("Exception in Taxation block");
         -- Check that the Text_IO file is open, then delete.
         if not Ada.Text_IO.Is_Open (Tax_Roll) then
            Ada.Text_IO.Open (Tax_Roll, 
                              Ada.Text_IO.Out_File, 
                              Tax_Roll_Name);
         end if;
         Ada.Text_IO.Delete (Tax_Roll);
   end Taxation;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAA010;
