-- CXA8003.A
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
--      Check that Append_File mode has not been added to package Direct_IO.
--
-- TEST DESCRIPTION:
--      This test uses a procedure to change the mode of an existing Direct_IO
--      file.  The file descriptor is passed as a parameter, along with a
--      numeric indicator for the new mode.  Based on the numeric parameter,
--      a Direct_IO.Reset is performed using a File_Mode'Value transformation
--      of a string constant into a File_Mode value.  An attempt to reset a 
--      Direct_IO file to mode Append_File should cause an Constraint_Error
--      to be raised, as Append_File mode has not been added to Direct_IO in
--      Ada 9X.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations supporting Direct_IO
--      files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Feb 97   PWB.CTA Allowed for non-support of Reset for certain 
--                  modes.
--!

with Direct_IO;
with Report;

procedure CXA8003 is
   Incomplete : exception;
      begin

         Report.Test ("CXA8003", "Check that Append_File mode has not " &
                                 "been added to package Direct_IO");

         Test_for_Direct_IO_Support:
         declare

            subtype String_Data_Type    is String (1 .. 20);
            type    Numeric_Data_Type   is range 1 .. 512;
            type    Composite_Data_Type is array (1 .. 3) of String_Data_Type;

            type File_Data_Type is record
               Data_Field_1 : String_Data_Type;
               Data_Field_2 : Numeric_Data_Type;
               Data_Field_3 : Composite_Data_Type;
            end record;

            package Dir_IO is new Direct_IO (File_Data_Type);

            Data_File    : Dir_IO.File_Type;
            Dir_Filename : constant String := Report.Legal_File_Name;

         begin

            -- An application creates a text file with mode Out_File.  
            -- Use_Error will be raised if Direct_IO operations or external 
            -- files are not supported.

            Dir_IO.Create (Data_File, 
                           Dir_IO.Out_File, 
                           Dir_Filename);

            Change_File_Mode:
            declare

               TC_Append_Test_Executed : Boolean := False;

               type Mode_Selection_Type is ( A, I, IO, O );

      
               procedure Change_Mode (File : in out Dir_IO.File_Type;
                                      To   : in     Mode_Selection_Type) is
               begin
                  case To is
                     when A  => 
                        TC_Append_Test_Executed := True;
                        Dir_IO.Reset 
                          (File, Dir_IO.File_Mode'Value("Append_File"));
                     when I  => 
                        begin
                          Dir_IO.Reset 
                            (File, Dir_IO.File_Mode'Value("In_File"));
                        exception
                          when Dir_IO.Use_Error =>
                            Report.Not_Applicable
                              ("Reset to In_File not supported: Direct_IO");
                            raise Incomplete;
                        end;
                     when IO => 
                        begin
                          Dir_IO.Reset 
                            (File, Dir_IO.File_Mode'Value("Inout_File"));
                        exception
                          when Dir_IO.Use_Error =>
                            Report.Not_Applicable
                              ("Reset to InOut_File not supported: Direct_IO");
                            raise Incomplete;
                        end;
                     when O  => 
                       begin
                         Dir_IO.Reset 
                           (File, Dir_IO.File_Mode'Value("Out_File"));
                        exception
                          when Dir_IO.Use_Error =>
                            Report.Not_Applicable
                              ("Reset to Out_File not supported: Direct_IO");
                            raise Incomplete;
                        end;
                  end case;
               end Change_Mode;


            begin

              -- At some point in the processing, the application may call a 
              -- procedure to change the mode of the file (perhaps for 
              -- additional data entry, data verification, etc.).  It is at 
              -- this point that a use of Append_File mode for a Direct_IO 
              -- file would cause an exception.

               for I in reverse Mode_Selection_Type loop
                  Change_Mode (Data_File, I);
                  Report.Comment 
                    ("Mode changed to " & 
                     Dir_IO.File_Mode'Image (Dir_IO.Mode (Data_File)));
               end loop;

               Report.Failed("No error raised on change to Append_File mode");

            exception

               -- A handler has been provided in the application, which 
               -- handles the constraint error, allowing processing to 
               -- continue.

               when Constraint_Error => 

                  if TC_Append_Test_Executed then 
                     Report.Comment ("Constraint_Error correctly raised on " &
                                     "attempted Append_File mode selection " &
                                     "for a Direct_IO file");
                  else
                     Report.Failed ("Append test was not executed");
                  end if;

               when Incomplete => raise;

               when others  => Report.Failed ("Unexpected exception raised");

            end Change_File_Mode;

            Final_Block:
            begin
              if Dir_IO.Is_Open (Data_File) then
                 Dir_IO.Delete (Data_File);
              else
                 Dir_IO.Open (Data_File, Dir_IO.In_File, Dir_Filename);
                 Dir_IO.Delete (Data_File);
              end if;
            exception
              when others =>
                Report.Failed ("Delete not properly supported: Direct_IO");
            end Final_Block;

         exception

            -- Since Use_Error or Name_Error can be raised if, for the 
            -- specified mode, the environment does not support Direct_IO 
            -- operations, the following handlers are included:

            when Dir_IO.Name_Error =>
               Report.Not_Applicable("Name_Error raised on Direct IO Create");

            when Dir_IO.Use_Error  =>
               Report.Not_Applicable("Use_Error raised on Direct IO Create");

            when others            =>
               Report.Failed 
                 ("Unexpected exception raised on Direct IO Create");

         end Test_for_Direct_IO_Support;

         Report.Result;

exception
  when Incomplete =>
    Report.Result;

end CXA8003;
