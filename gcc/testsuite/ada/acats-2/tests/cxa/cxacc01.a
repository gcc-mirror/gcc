-- CXACC01.A
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
--      Check that the use of 'Class'Output and 'Class'Input allow stream 
--      manipulation of objects of non-limited class-wide types.
--
-- TEST DESCRIPTION:
--      This test demonstrates the uses of 'Class'Output and 'Class'Input
--      in moving objects of a particular class to and from a stream file.
--      A procedure uses a class-wide parameter to move objects of specific 
--      types in the class to the stream, using the 'Class'Output attribute 
--      of the root type of the class.  A function returns a class-wide object, 
--      using the 'Class'Input attribute of the root type of the class to 
--      extract the object from the stream.
--      A field-by-field comparison of record objects is performed to validate
--      the data read from the stream.  Operator precedence rules are used
--      in the comparison rather than parentheses.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations capable of supporting
--      external Stream_IO files.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      14 Nov 95   SAIC    Corrected prefix of 'Tag attribute for ACVC 2.0.1.
--      24 Aug 96   SAIC    Changed a call to "Create" to "Reset".
--      26 Feb 97   CTA.PWB Allowed for non-support of some IO operations.
--!

with FXACC00, Ada.Streams.Stream_IO, Ada.Tags, Report;

procedure CXACC01 is

   Order_File     : Ada.Streams.Stream_IO.File_Type;
   Order_Stream   : Ada.Streams.Stream_IO.Stream_Access;
   Order_Filename : constant String := 
                           Report.Legal_File_Name ( Nam => "CXACC01" );
   Incomplete : exception;

begin

   Report.Test ("CXACC01", "Check that the use of 'Class'Output "        &
                           "and 'Class'Input allow stream manipulation " &
                           "of objects of non-limited class-wide types");

   Test_for_Stream_IO_Support:
   begin

      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on 
      -- calls to various Stream_IO operations.  This block statement 
      -- encloses a call to Create, which should produce an exception in a 
      -- non-supportive environment.  These exceptions will be handled to 
      -- produce a Not_Applicable result.

      Ada.Streams.Stream_IO.Create (Order_File, 
                                    Ada.Streams.Stream_IO.Out_File,
                                    Order_Filename);

   exception

       when Ada.Streams.Stream_IO.Use_Error | Ada.Streams.Stream_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Stream_IO" );
          raise Incomplete;

   end Test_for_Stream_IO_Support;

   Operational_Test_Block:
   declare

      -- Store tag values associated with objects of tagged types.

      TC_Box_Office_Tag : constant String :=
        Ada.Tags.External_Tag(FXACC00.Ticket_Request'Tag);

      TC_Summer_Tag     : constant String :=
        Ada.Tags.External_Tag(FXACC00.Subscriber_Request'Tag);

      TC_Mayoral_Tag    : constant String :=
        Ada.Tags.External_Tag(FXACC00.VIP_Request'Tag);

      TC_Late_Tag       : constant String :=
        Ada.Tags.External_Tag(FXACC00.Last_Minute_Request'Tag);

         -- The following procedure will take an object of the Ticket_Request
         -- class and output it to the stream.  Objects of any extended type
         -- in the class can be output to the stream with this procedure.

      procedure Order_Entry (Order : FXACC00.Ticket_Request'Class) is
      begin
         FXACC00.Ticket_Request'Class'Output (Order_Stream, Order);
      end Order_Entry;


      -- The following function will retrieve from the stream an object of
      -- the Ticket_Request class.

      function Order_Retrieval return FXACC00.Ticket_Request'Class is
      begin
         return FXACC00.Ticket_Request'Class'Input (Order_Stream);
      end Order_Retrieval;

   begin

      Order_Stream := Ada.Streams.Stream_IO.Stream (Order_File);

      -- Store the data objects in the stream.
      -- Each of the objects is of a different type within the class.

      Order_Entry (FXACC00.Box_Office_Request);     -- Object of root type
      Order_Entry (FXACC00.Summer_Subscription);    -- Obj. of extended type
      Order_Entry (FXACC00.Mayoral_Ticket_Request); -- Obj. of extended type
      Order_Entry (FXACC00.Late_Request);           -- Object of twice 
                                                    -- extended type.

      -- Reset mode of stream to In_File prior to reading data from it.
      Reset1:
      begin
         Ada.Streams.Stream_IO.Reset (Order_File, 
                                      Ada.Streams.Stream_IO.In_File);
      exception
         when Ada.Streams.Stream_IO.Use_Error =>
            Report.Not_Applicable
               ( "Reset to In_File not supported for Stream_IO - 1" );
            raise Incomplete;
      end Reset1;

      Process_Order_Block:
      declare

         use FXACC00;

         -- Declare variables of the root type class,
         -- and initialize them with class-wide objects returned from
         -- the stream as function result.

         Order_1 : Ticket_Request'Class := Order_Retrieval;
         Order_2 : Ticket_Request'Class := Order_Retrieval;
         Order_3 : Ticket_Request'Class := Order_Retrieval;
         Order_4 : Ticket_Request'Class := Order_Retrieval;

         -- Declare objects of the specific types from within the class
         -- that correspond to the types of the data written to the
         -- stream.  Perform a type conversion on the class-wide objects.

         Ticket_Order      : Ticket_Request := 
                                Ticket_Request(Order_1);
         Subscriber_Order  : Subscriber_Request := 
                                Subscriber_Request(Order_2);
         VIP_Order         : VIP_Request := 
                                VIP_Request(Order_3);
         Last_Minute_Order : Last_Minute_Request := 
                                Last_Minute_Request(Order_4);

      begin

         -- Perform a field-by-field comparison of all the class-wide 
         -- objects input from the stream with specific type objects
         -- originally written to the stream.

         if Ticket_Order.Location                /= 
            Box_Office_Request.Location          or
            Ticket_Order.Number_Of_Tickets       /=
            Box_Office_Request.Number_Of_Tickets 
         then
            Report.Failed ("Ticket_Request object validation failure");
         end if;

         if Subscriber_Order.Location               /=
            Summer_Subscription.Location            or
            Subscriber_Order.Number_Of_Tickets      /=
            Summer_Subscription.Number_Of_Tickets   or
            Subscriber_Order.Subscription_Number    /=
            Summer_Subscription.Subscription_Number
         then
            Report.Failed ("Subscriber_Request object validation failure");
         end if;

         if VIP_Order.Location                       /=
            Mayoral_Ticket_Request.Location          or
            VIP_Order.Number_Of_Tickets              /=
            Mayoral_Ticket_Request.Number_Of_Tickets or
            VIP_Order.Rank                           /=
            Mayoral_Ticket_Request.Rank              
         then
            Report.Failed ("VIP_Request object validation failure");
         end if;

         if Last_Minute_Order.Location               /=
            Late_Request.Location                    or
            Last_Minute_Order.Number_Of_Tickets      /=
            Late_Request.Number_Of_Tickets           or
            Last_Minute_Order.Rank                   /=
            Late_Request.Rank                        or
            Last_Minute_Order.Special_Consideration  /=
            Late_Request.Special_Consideration       or
            Last_Minute_Order.Donation               /=
            Late_Request.Donation                    
         then
            Report.Failed ("Last_Minute_Request object validation failure");
         end if;

         -- Verify tag values from before and after processing.
         -- The 'Tag attribute is used with objects of a class-wide type.
   
         if TC_Box_Office_Tag /= 
            Ada.Tags.External_Tag(Order_1'Tag)
         then
            Report.Failed("Failed tag comparison - 1");
         end if;
     
         if TC_Summer_Tag /=
            Ada.Tags.External_Tag(Order_2'Tag)
         then
            Report.Failed("Failed tag comparison - 2");
         end if;

         if TC_Mayoral_Tag /=  
            Ada.Tags.External_Tag(Order_3'Tag)
         then
            Report.Failed("Failed tag comparison - 3");
         end if;

         if TC_Late_Tag /=     
            Ada.Tags.External_Tag(Order_4'Tag)
         then
            Report.Failed("Failed tag comparison - 4");
         end if;

      end Process_Order_Block;

         -- After all the data has been correctly extracted, the file 
         -- should be empty.

      if not Ada.Streams.Stream_IO.End_Of_File (Order_File) then
         Report.Failed ("Stream file not empty");
      end if;

   exception
      when Incomplete =>
         raise; 
      when Constraint_Error =>
         Report.Failed ("Constraint_Error raised in Operational Block");
      when others => 
         Report.Failed ("Exception raised in Operational Test Block");
   end Operational_Test_Block;

   Deletion:
   begin
      if Ada.Streams.Stream_IO.Is_Open (Order_File) then
         Ada.Streams.Stream_IO.Delete (Order_File);
      else
         Ada.Streams.Stream_IO.Open (Order_File,
                                     Ada.Streams.Stream_IO.Out_File, 
                                     Order_Filename);
         Ada.Streams.Stream_IO.Delete (Order_File);
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

end CXACC01;
