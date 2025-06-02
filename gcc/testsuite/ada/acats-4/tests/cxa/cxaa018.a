-- CXAA018.A
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
--      Check that the subprograms defined in the package Text_IO.Modular_IO
--      provide correct results.  
--
-- TEST DESCRIPTION:
--      This test checks that the subprograms defined in the 
--      Ada.Text_IO.Modular_IO package provide correct results.  
--      A modular type is defined and used to instantiate the generic
--      package Ada.Text_IO.Modular_IO.  Values of the modular type are
--      written to a Text_IO file, and to a series of string variables, using
--      different versions of the procedure Put from the instantiated IO
--      package.  These modular data items are retrieved from the file and
--      string variables using the appropriate instantiated version of
--      procedure Get.  A variety of Base and Width parameter values are
--      used in the procedure calls.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that support Text_IO
--      processing and external files.
--
--       
-- CHANGE HISTORY:
--      03 Jul 95   SAIC    Initial prerelease version.
--      01 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--
--!

with Ada.Text_IO;
with System;
with Report;

procedure CXAA018 is
begin

   Report.Test ("CXAA018", "Check that the subprograms defined in "  &
                           "the package Text_IO.Modular_IO provide " & 
                           "correct results");

   Test_for_Text_IO_Support:
   declare
      Data_File     : Ada.Text_IO.File_Type;
      Data_Filename : constant String := Report.Legal_File_Name;
   begin

      -- An application creates a text file in mode Out_File, with the 
      -- intention of entering modular data into the file as appropriate.  
      -- In the event that the particular environment where the application 
      -- is running does not support Text_IO, Use_Error or Name_Error will be 
      -- raised on calls to Text_IO operations.  Either of these exceptions
      -- will be handled to produce a Not_Applicable result.

      Ada.Text_IO.Create (File => Data_File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Data_Filename);

      Test_Block:
      declare

         type Mod_Type is mod System.Max_Binary_Modulus; 
         -- Max_Binary_Modulus must be at least 2**16, which would result
         -- in a base range of 0..65535 (zero to one less than the given
         -- modulus) for this modular type.

         package Mod_IO is new Ada.Text_IO.Modular_IO(Mod_Type);
         use Ada.Text_IO, Mod_IO;
         use type Mod_Type;

         Number_Of_Modular_Items : constant := 6;
         Number_Of_Error_Items   : constant := 1;

         TC_Modular              : Mod_Type;
         TC_Last_Character_Read  : Positive;

         Modular_Array : array (1..Number_Of_Modular_Items) of Mod_Type := 
                                   ( 0, 97, 255, 1025, 12097, 65535 );


         procedure Load_File (The_File : in out Ada.Text_IO.File_Type) is
         begin
            -- This procedure does not create, open, or close the data file;
            -- The_File file object must be Open at this point.
            -- This procedure is designed to load Modular_Type data into a 
            -- data file. 
            --
            -- Use the Modular_IO procedure Put to enter modular data items
            -- into the data file.

            for i in 1..Number_Of_Modular_Items loop
               -- Use default Base parameter of 10.
               Mod_IO.Put(File  => Data_File, 
                          Item  => Modular_Array(i), 
                          Width => 6, 
                          Base  => Mod_IO.Default_Base);
            end loop;

            -- Enter data into the file such that on the corresponding "Get"
            -- of this data, Data_Error must be raised.  This value is outside
            -- the base range of Modular_Type.
            -- Text_IO is used to enter the value in the file.

            for i in 1..Number_Of_Error_Items loop
               Ada.Text_IO.Put(The_File, "-10");  
            end loop;

         end Load_File;



         procedure Process_File(The_File : in out Ada.Text_IO.File_Type) is
         begin
            -- This procedure does not create, open, or close the data file;
            -- The_File file object must be Open at this point.
            -- Use procedure Get (for Files) to extract the modular data from
            -- the Text_IO file.  

            for i in 1..Number_Of_Modular_Items loop
               Mod_IO.Get(The_File, TC_Modular, Width => 6);

               if TC_Modular /= Modular_Array(i) then
                  Report.Failed("Incorrect modular data read from file " &
                                "data item #" & Integer'Image(i));
               end if;
            end loop;

            -- The final item in the Data_File is a modular value that is
            -- outside the base range 0..Num'Last.  This value should raise
            -- Data_Error on an attempt to "Get" it from the file.

            for i in 1..Number_Of_Error_Items loop
               begin
                  Mod_IO.Get(The_File, TC_Modular, Mod_IO.Default_Width);
                  Report.Failed
                    ("Exception Data_Error not raised when Get "   &
                     "was used to read modular data outside base " &
                     "range of type, item # "                      & 
                     Integer'Image(i));
               exception
                  when Ada.Text_IO.Data_Error => 
                     null; -- OK, expected exception.
                  when others =>
                     Report.Failed("Unexpected exception raised when Get "  &
                                   "was used to read modular data outside " &
                                   "base range of type from Data_File, "    &
                                   "data item #" & Integer'Image(i));
               end;
            end loop;
       
         exception
            when others =>
              Report.Failed
                ("Unexpected exception raised in Process_File");
         end Process_File;



      begin  -- Test_Block.

         -- Place modular values into data file.

         Load_File(Data_File);
         Ada.Text_IO.Close(Data_File);

         -- Read modular values from data file.

         Ada.Text_IO.Open(Data_File, Ada.Text_IO.In_File, Data_Filename);
         Process_File(Data_File);

         -- Verify versions of Modular_IO procedures Put and Get for Strings.

         Modular_IO_in_Strings:
         declare
            TC_String_Array : array (1..Number_Of_Modular_Items) 
                              of String(1..30) := (others =>(others => ' '));
         begin

            -- Place modular values into strings using the Procedure Put, 
            -- Use a variety of different "Base" parameter values.
            -- Note: This version of Put uses the length of the given
            --       string as the value of the "Width" parameter.

            for i in 1..2 loop
               Mod_IO.Put(To   => TC_String_Array(i),
                          Item => Modular_Array(i),
                          Base => Mod_IO.Default_Base);
            end loop;
            for i in 3..4 loop
               Mod_IO.Put(TC_String_Array(i),
                          Modular_Array(i),
                          Base => 2);
            end loop;
            for i in 5..6 loop
               Mod_IO.Put(TC_String_Array(i), Modular_Array(i), 16);
            end loop;

            -- Get modular values from strings using the Procedure Get.
            -- Compare with expected modular values.

            for i in 1..Number_Of_Modular_Items loop

               Mod_IO.Get(From => TC_String_Array(i),
                          Item => TC_Modular,
                          Last => TC_Last_Character_Read);

               if TC_Modular /= Modular_Array(i) then
                  Report.Failed("Incorrect modular data value obtained "   &
                                "from String following use of Procedures " & 
                                "Put and Get from Strings, Modular_Array " &
                                "item #" & Integer'Image(i));
               end if;
            end loop;
         
         exception
            when others => 
               Report.Failed("Unexpected exception raised during the " &
                             "evaluation of Put and Get for Strings");
         end Modular_IO_in_Strings;

      exception
         when others => Report.Failed ("Exception raised in Test_Block");
      end Test_Block;


      -- Delete the external file.
      if Ada.Text_IO.Is_Open(Data_File) then
         Ada.Text_IO.Delete(Data_File);
      else
         Ada.Text_IO.Open(Data_File, 
                          Ada.Text_IO.In_File,
                          Data_Filename);
         Ada.Text_IO.Delete(Data_File);
      end if;

   exception

      -- Since Use_Error can be raised if, for the specified mode,
      -- the environment does not support Text_IO operations, the 
      -- following handlers are included:

      when Ada.Text_IO.Use_Error  =>
         Report.Not_Applicable ("Use_Error raised on Text_IO Create");

      when Ada.Text_IO.Name_Error =>
         Report.Not_Applicable ("Name_Error raised on Text_IO Create");

      when others                 =>
         Report.Failed ("Unexpected exception raised on text file Create");

   end Test_for_Text_IO_Support;

   Report.Result;

end CXAA018;
