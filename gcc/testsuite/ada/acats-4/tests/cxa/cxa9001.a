-- CXA9001.A
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
--      Check that the operations defined in the generic package 
--      Ada.Storage_IO provide the ability to store and retrieve objects 
--      which may include implicit levels of indirection in their 
--      implementation, from an in-memory buffer.
--
-- TEST DESCRIPTION:
--      The following scenario demonstrates how an object of a type with 
--      (potential) levels of indirection (based on the implementation) 
--      can be "flattened" and written/read to/from a Direct_IO file.
--      In this small example, we have attempted to simulate the situation 
--      where two independent programs are using a particular Direct_IO file,
--      one writing data to the file, and the second program reading that file.
--      The Storage_IO Read and Write procedures are used to "flatten" 
--      and reconstruct objects of the record type. 
--
-- APPLICABILITY CRITERIA: 
--      Applicable to implementations capable of supporting external
--      Direct_IO files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      07 Jun 95   SAIC    Modified to constrain type used with Storage_IO.
--      20 Nov 95   SAIC    Corrected and enhanced for ACVC 2.0.1.
--      25 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--!

with Report;
with Ada.Storage_IO;
with Ada.Direct_IO;

procedure CXA9001 is
   package Dir_IO is new Ada.Direct_IO (Integer);
   Test_File  : Dir_IO.File_Type;
   Incomplete : exception;
begin

   Report.Test ("CXA9001", "Check that the operations defined in the "      &
                           "generic package Ada.Storage_IO provide the "    &
                           "ability to store and retrieve objects which "   &
                           "may include implicit levels of indirection in " &
                           "their implementation, from an in-memory buffer");


   Test_For_Direct_IO_Support:
   begin

      -- The following Create does not have any bearing on the test scenario,
      -- but is included to check that the implementation supports Direct_IO
      -- files.  An exception on this Create statement will raise a Name_Error
      -- or Use_Error, which will be handled to produce a Not_Applicable
      -- result. If created, the file is immediately deleted, as it is not 
      -- needed for the program scenario.

      Dir_IO.Create (Test_File, Dir_IO.Out_File, Report.Legal_File_Name(1));

   exception

       when Dir_IO.Use_Error | Dir_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Direct_IO" );
          raise Incomplete;

   end Test_for_Direct_IO_Support;

   Deletion1:
   begin
      Dir_IO.Delete (Test_File);
   exception
      when others =>
         Report.Failed
            ( "Delete not properly implemented for Direct_IO - 1" );
   end Deletion1;


   Test_Block:
   declare

      The_Filename : constant String := Report.Legal_File_Name(2);

      -- The following type is the basic unit used in this test. It is
      -- incorporated into the definition of the Unit_Array_Type.

      type Unit_Type is
         record
            Position     : Natural := 19;
            String_Value : String (1..9) := (others => 'X');
         end record;

      TC_Size : Natural := Natural'First;

      procedure Data_Storage (Number_Of_Units  : in     Natural;
                              Result           :    out Natural) is

         -- Type based on input parameter.  Uses type Unit_Type 
         -- as the array element.
         type Unit_Array_Type is array (1..Number_Of_Units) 
           of Unit_Type; 

         -- This type definition is the ultimate storage type used
         -- in this test; uses type Unit_Array_Type as a record
         -- component field.
         -- This record type contains a component that is an array of
         -- records, with each of these records containing a Natural
         -- and a String value (i.e., a record containing an array of
         -- records).

         type Data_Storage_Type is
            record
               Data_Value : Natural := Number_Of_Units;
               Unit_Array : Unit_Array_Type;
            end record;

         -- The instantiation of the following generic package is a
         -- central point in this test.  Storage_IO is instantiated for
         -- a specific data type, and will be used to "flatten" objects
         -- of that type into buffers.  Direct_IO is instantiated for
         -- these Storage_IO buffers.

         package Flat_Storage_IO is 
           new Ada.Storage_IO (Data_Storage_Type);
         package Buffer_IO is 
           new Ada.Direct_IO (Flat_Storage_IO.Buffer_Type);

         Buffer_File      : Buffer_IO.File_Type;
         Outbound_Buffer  : Flat_Storage_IO.Buffer_Type;
         Storage_Item     : Data_Storage_Type;

      begin  -- procedure Data_Storage

         Buffer_IO.Create (Buffer_File, 
                           Buffer_IO.Out_File, 
                           The_Filename);

         Flat_Storage_IO.Write (Buffer => Outbound_Buffer, 
                                Item   => Storage_Item);

         -- At this point, any levels of indirection have been removed
         -- by the Storage_IO procedure, and the buffered data can be
         -- written to a file.

         Buffer_IO.Write (Buffer_File, Outbound_Buffer);
         Buffer_IO.Close (Buffer_File);
         Result := Storage_Item.Unit_Array'Last +           -- 5 +
                   Storage_Item.Unit_Array                  -- 9
                     (Storage_Item.Unit_Array'First).String_Value'Length;

      exception
         when others => 
            Report.Failed ("Data storage error");
            if Buffer_IO.Is_Open (Buffer_File) then
               Buffer_IO.Close (Buffer_File);
            end if;       
      end Data_Storage;

      procedure Data_Retrieval (Number_Of_Units  : in     Natural;
                                Result           :    out Natural) is
         type Unit_Array_Type is array (1..Number_Of_Units) 
           of Unit_Type;

         type Data_Storage_Type is
            record
               Data_Value : Natural := Number_Of_Units;
               Unit_Array : Unit_Array_Type;
            end record;

         package Flat_Storage_IO is 
           new Ada.Storage_IO (Data_Storage_Type);
         package Reader_IO is 
           new Ada.Direct_IO (Flat_Storage_IO.Buffer_Type);

         Reader_File      : Reader_IO.File_Type;
         Inbound_Buffer   : Flat_Storage_IO.Buffer_Type;
         Storage_Item     : Data_Storage_Type; 
         TC_Item          : Data_Storage_Type; 

      begin  -- procedure Data_Retrieval

         Reader_IO.Open (Reader_File, Reader_IO.In_File, The_Filename);
         Reader_IO.Read (Reader_File, Inbound_Buffer);

         Flat_Storage_IO.Read (Inbound_Buffer, Storage_Item);

         -- Validate the reconstructed value against an "unflattened"
         -- value.

         if Storage_Item.Data_Value /= TC_Item.Data_Value 
         then
            Report.Failed ("Data_Retrieval Error - 1");
         end if;

         for i in 1..Number_Of_Units loop
            if Storage_Item.Unit_Array(i).String_Value'Length  /=
               TC_Item.Unit_Array(i).String_Value'Length      or
               Storage_Item.Unit_Array(i).Position     /=
               TC_Item.Unit_Array(i).Position             or
               Storage_Item.Unit_Array(i).String_Value /=
               TC_Item.Unit_Array(i).String_Value      
            then
               Report.Failed ("Data_Retrieval Error - 2");
            end if;
         end loop;

         Result := Storage_Item.Unit_Array'Last +           -- 5 +
                   Storage_Item.Unit_Array                  -- 9
                     (Storage_Item.Unit_Array'First).String_Value'Length;

         if Reader_IO.Is_Open (Reader_File) then
            Reader_IO.Delete (Reader_File);
         else
            Reader_IO.Open (Reader_File, 
                            Reader_IO.In_File, 
                            The_Filename);
            Reader_IO.Delete (Reader_File);
         end if;       

      exception
         when others => 
            Report.Failed ("Exception raised in Data_Retrieval");
            if Reader_IO.Is_Open (Reader_File) then
               Reader_IO.Delete (Reader_File);
            else
               Reader_IO.Open (Reader_File, 
                               Reader_IO.In_File, 
                               The_Filename);
               Reader_IO.Delete (Reader_File);
            end if;       
      end Data_Retrieval;


   begin  -- Test_Block

      -- The number of Units is provided in this call to Data_Storage.
      Data_Storage (Number_Of_Units  => Natural(Report.Ident_Int(5)), 
                    Result           => TC_Size);

      if TC_Size /= 14 then
         Report.Failed ("Data_Storage error in Data_Storage");
      end if;

      Data_Retrieval (Number_Of_Units  => Natural(Report.Ident_Int(5)), 
                      Result           => TC_Size);

      if TC_Size /= 14 then
         Report.Failed ("Data retrieval error in Data_Retrieval");
      end if;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXA9001;
