-- CXACB02.A
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
--      Check that user defined subprograms can override the default
--      attributes 'Input and 'Output using attribute definition clauses,
--      when used with objects of discriminated record and multi-dimensional
--      array types.
--
-- TEST DESCRIPTION:
--      This test demonstrates that the default implementations of the
--      'Input and 'Output attributes can be overridden by user specified 
--      subprograms in conjunction with attribute definition clauses.
--      These attributes have been overridden below, and in the user defined
--      substitutes, values are added or subtracted to global variables.
--      Following the completion of the writing/reading test, the global
--      variables are evaluated to ensure that the user defined subprograms
--      were used in overriding the type-related default attributes.
--      
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations that support external
--      Stream_IO files.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      14 Nov 95   SAIC    Corrected test errors for ACVC 2.0.1.
--
--!

with Report;
with Ada.Streams.Stream_IO;

procedure CXACB02 is
begin

   Report.Test ("CXACB02", "Check that user defined subprograms can "    &
                           "override the default attributes 'Input and " &
                           "'Output using attribute definition clauses");

   Test_for_Stream_IO_Support:
   declare

      Util_File        : Ada.Streams.Stream_IO.File_Type;
      Util_Stream      : Ada.Streams.Stream_IO.Stream_Access;
      Utility_Filename : constant String := Report.Legal_File_Name;

   begin

      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on 
      -- calls to various Stream_IO operations.  This block statement 
      -- encloses a call to Create, which should produce an exception in a 
      -- non-supportive environment.  These exceptions will be handled to 
      -- produce a Not_Applicable result.

      Ada.Streams.Stream_IO.Create (Util_File, 
                                    Ada.Streams.Stream_IO.Out_File,
                                    Utility_Filename);

      Operational_Test_Block:
      declare

         type    Customer_Type          is (Residence, Apartment, Commercial);
         type    Electric_Usage_Type    is range 0..100000;
         type    Months_In_Service_Type is range 1..12;
         type    Quarterly_Period_Type  is (Spring, Summer, Autumn, Winter);
         subtype Month_In_Quarter_Type  is Positive range 1..3;
         type    Service_History_Type   is 
           array (Quarterly_Period_Type range <>, 
                  Month_In_Quarter_Type range <>) of Electric_Usage_Type;

         type Service_Type (Customer : Customer_Type) is
            record
               Name       : String (1..21);               
               Account_ID : Natural range 0..100;
               case Customer is
                  when Residence | Apartment =>
                     Low_Income_Credit : Boolean := False;
                  when Commercial            =>
                     Baseline_Allowance : Natural range 0..1000;
                     Quantity_Discount  : Boolean := False;
               end case;
            end record;


         -- Mode conformant, user defined subprograms that will override 
         -- the type-related attributes.
         -- In this test, the user defines these subprograms to add/subtract
         -- specific values from global variables.

         function Service_Input 
           (Stream : access Ada.Streams.Root_Stream_Type'Class) 
           return Service_Type;

         procedure Service_Output 
           (Stream : access Ada.Streams.Root_Stream_Type'Class;
            Item   : Service_Type);

         function History_Input 
           (Stream : access Ada.Streams.Root_Stream_Type'Class)
           return Service_History_Type;

         procedure History_Output
           (Stream : access Ada.Streams.Root_Stream_Type'Class;
            Item   : Service_History_Type);


         -- Attribute definition clauses.

         for Service_Type'Input  use Service_Input;
         for Service_Type'Output use Service_Output;

         for Service_History_Type'Input  use History_Input;
         for Service_History_Type'Output use History_Output;


         -- Object Declarations

         Customer1 : Service_Type (Residence) := 
                       (Residence, "1221 Morningstar Lane", 44, False); 
         Customer2 : Service_Type (Apartment) := 
                       (Customer => Apartment,
                        Account_ID => 67,
                        Name => "15 South Front St. #8",
                        Low_Income_Credit => True);
         Customer3 : Service_Type (Commercial) := 
                       (Commercial,
                        "12442 Central Avenue ", 
                        100, 
                        Baseline_Allowance => 938, 
                        Quantity_Discount  => True);

         C1_Service_History : 
           Service_History_Type (Quarterly_Period_Type, 
                                 Month_In_Quarter_Type) := 
             (Spring => (1 => 35, 2 => 39, 3 => 32),
              Summer => (1 => 34, 2 => 33, 3 => 39),
              Autumn => (1 => 45, 2 => 40, 3 => 38),
              Winter => (1 => 53, 2 =>  0, 3 =>  0));
  
         C2_Service_History : 
           Service_History_Type (Quarterly_Period_Type range Spring..Summer, 
                                 Month_In_Quarter_Type) := 
             (Spring => (23, 22, 0), Summer => (0, 0, 0));

         C3_Service_History :
           Service_History_Type (Quarterly_Period_Type, 
                                 Month_In_Quarter_Type) := 
             (others => (others => 200));

   
         TC_Input_Total  : Integer := 0;
         TC_Output_Total : Integer := 0;


         -- Subprogram bodies.
         -- These subprograms are designed to override the default attributes
         -- 'Input and 'Output for the specified types.  Each adds/subtracts
         -- a quantity to/from a program control variable, indicating its
         -- activity.  Each user defined "Input" function uses the 'Read
         -- attribute for the type to accomplish the operation.  Likewise, 
         -- each user defined "Output" subprogram uses the 'Write attribute 
         -- for the type.

         function Service_Input
           ( Stream : access Ada.Streams.Root_Stream_Type'Class )
           return Service_Type is
            Customer : Customer_Type;
         begin
            TC_Input_Total := TC_Input_Total + 1;

            -- Extract the discriminant value from the stream.
            -- This discriminant would not otherwise be extracted from the
            -- stream when the Service_Type'Read attribute is used below.
            Customer_Type'Read (Stream, Customer);

            declare
               -- Declare a constant of Service_Type, using the value just
               -- read from the stream as the discriminant value of the
               -- object.
               Service : Service_Type(Customer);
            begin
               Service_Type'Read (Stream, Service);
               return Service;
            end;
         end Service_Input;


        procedure Service_Output
           ( Stream : access Ada.Streams.Root_Stream_Type'Class;
             Item   : Service_Type ) is
         begin
            TC_Output_Total := TC_Output_Total + 2;
            -- Write the discriminant value to the stream.
            -- The attribute 'Write (for the record type) will not write the 
            -- discriminant of the record object to the stream.  Therefore, it
            -- must be explicitly written using the 'Write attribute of the
            -- discriminant type.
            Customer_Type'Write (Stream, Item.Customer);
            -- Write the record component values (but not the discriminant) to 
            -- the stream.
            Service_Type'Write (Stream, Item);
         end Service_Output;


         function History_Input
           ( Stream : access Ada.Streams.Root_Stream_Type'Class )
           return Service_History_Type is
           Quarter_Bound_Low  : Quarterly_Period_Type;
           Quarter_Bound_High : Quarterly_Period_Type;
           Month_Bound_Low    : Month_In_Quarter_Type;
           Month_Bound_High   : Month_In_Quarter_Type;
         begin
            TC_Input_Total := TC_Input_Total + 3;

            -- Read the value of the array bounds from the stream.
            -- Use these bounds in the creation of an array object that will
            -- be used to store data from the stream.
            -- The array bound values would not otherwise be read from the 
            -- stream by use of the Service_History_Type'Read attribute.
            Quarterly_Period_Type'Read (Stream, Quarter_Bound_Low);
            Quarterly_Period_Type'Read (Stream, Quarter_Bound_High);
            Month_In_Quarter_Type'Read (Stream, Month_Bound_Low);
            Month_In_Quarter_Type'Read (Stream, Month_Bound_High);

            declare
               Service_History_Array : 
                 Service_History_Type 
                 (Quarterly_Period_Type range
                    Quarter_Bound_Low..Quarter_Bound_High, 
                  Month_In_Quarter_Type range
                    Month_Bound_Low .. Month_Bound_High);
            begin
               Service_History_Type'Read (Stream, Service_History_Array);
               return Service_History_Array;
            end;
         end History_Input;


         procedure History_Output
           ( Stream : access Ada.Streams.Root_Stream_Type'Class;
             Item   : Service_History_Type ) is
         begin
            TC_Output_Total := TC_Output_Total + 7;
            -- Write the upper/lower bounds of the array object dimensions to 
            -- the stream.
            Quarterly_Period_Type'Write (Stream, Item'First(1));
            Quarterly_Period_Type'Write (Stream, Item'Last(1));
            Month_In_Quarter_Type'Write (Stream, Item'First(2));
            Month_In_Quarter_Type'Write (Stream, Item'Last(2));
            -- Write the array values to the stream in canonical order (last
            -- dimension varying fastest).
            Service_History_Type'Write (Stream, Item);
         end History_Output;



      begin

         Util_Stream := Ada.Streams.Stream_IO.Stream (Util_File);

         -- Write data to the stream. A customer service record is followed 
         -- by a service history array.

         Service_Type'Output (Util_Stream, Customer1);
         Service_History_Type'Output (Util_Stream, C1_Service_History);

         Service_Type'Output (Util_Stream, Customer2);
         Service_History_Type'Output (Util_Stream, C2_Service_History);

         Service_Type'Output (Util_Stream, Customer3);
         Service_History_Type'Output (Util_Stream, C3_Service_History);


         -- Read data from the stream, and verify the use of the user specified
         -- attributes.

         Verify_Data_Block:
         declare

            TC_Residence  : Service_Type (Residence);
            TC_Apartment  : Service_Type (Apartment);
            TC_Commercial : Service_Type (Commercial);

            TC_History1   : Service_History_Type (Quarterly_Period_Type, 
                                                  Month_In_Quarter_Type) :=
              (others => (others => Electric_Usage_Type'First));

            TC_History2   : Service_History_Type (Quarterly_Period_Type 
                                                    range Spring .. Summer,
                                                  Month_In_Quarter_Type) :=
              (others => (others => Electric_Usage_Type'First));

            TC_History3   : Service_History_Type (Quarterly_Period_Type,
                                                  Month_In_Quarter_Type) :=
              (others => (others => Electric_Usage_Type'First));

         begin

            -- Reset Stream file to mode In_File.

            Ada.Streams.Stream_IO.Reset (Util_File, 
                                         Ada.Streams.Stream_IO.In_File);

            -- Read data from the stream.

            TC_Residence  := Service_Type'Input (Util_Stream);
            TC_History1   := Service_History_Type'Input (Util_Stream);

            TC_Apartment  := Service_Type'Input (Util_Stream);
            TC_History2   := Service_History_Type'Input (Util_Stream);

            TC_Commercial := Service_Type'Input (Util_Stream);
            TC_History3   := Service_History_Type'Input (Util_Stream);


            -- Verify product data was correctly written to/read from stream,
            -- including discriminants and array bounds.

            if (TC_Residence          /= Customer1)                   or
               (TC_Residence.Customer /= Customer1.Customer)          or 
               (TC_History1'Last(1)   /= C1_Service_History'Last(1))  or
               (TC_History1'First(1)  /= C1_Service_History'First(1)) or
               (TC_History1'Last(2)   /= C1_Service_History'Last(2))  or
               (TC_History1'First(2)  /= C1_Service_History'First(2)) 
            then
               Report.Failed ("Incorrect data from stream - 1");
            end if;

            if (TC_Apartment          /= Customer2)                   or
               (TC_Apartment.Customer /= Customer2.Customer)          or 
               (TC_History2           /= C2_Service_History)          or
               (TC_History2'Last(1)   /= C2_Service_History'Last(1))  or
               (TC_History2'First(1)  /= C2_Service_History'First(1)) or
               (TC_History2'Last(2)   /= C2_Service_History'Last(2))  or
               (TC_History2'First(2)  /= C2_Service_History'First(2)) 
            then
               Report.Failed ("Incorrect data from stream - 2");
            end if;

            if (TC_Commercial          /= Customer3)                   or
               (TC_Commercial.Customer /= Customer3.Customer)          or 
               (TC_History3            /= C3_Service_History)          or
               (TC_History3'Last(1)    /= C3_Service_History'Last(1))  or
               (TC_History3'First(1)   /= C3_Service_History'First(1)) or
               (TC_History3'Last(2)    /= C3_Service_History'Last(2))  or
               (TC_History3'First(2)   /= C3_Service_History'First(2)) 
            then
               Report.Failed ("Incorrect data from stream - 3");
            end if;

            -- Verify that the user defined subprograms were used to override
            -- the default 'Input and 'Output attributes.
            -- There were three calls on each of the user defined attributes.
            
            if (TC_Input_Total /= 12 ) or (TC_Output_Total /= 27 ) then
               Report.Failed ("Incorrect use of user defined attributes");
            end if;

         end Verify_Data_Block;

      exception

         when others => 
            Report.Failed ("Exception raised in Operational Test Block");

      end Operational_Test_Block;

      if Ada.Streams.Stream_IO.Is_Open (Util_File) then
         Ada.Streams.Stream_IO.Delete (Util_File);
      else
         Ada.Streams.Stream_IO.Open (Util_File,
                                     Ada.Streams.Stream_IO.Out_File, 
                                     Utility_Filename);
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

end CXACB02;
