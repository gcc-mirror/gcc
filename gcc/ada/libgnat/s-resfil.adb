------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . R E S P O N S E _ F I L E                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2007-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Compiler_Unit_Warning;

with Ada.Unchecked_Deallocation;

with System.OS_Lib; use System.OS_Lib;

package body System.Response_File is

   type File_Rec;
   type File_Ptr is access File_Rec;
   type File_Rec is record
      Name : String_Access;
      Next : File_Ptr;
      Prev : File_Ptr;
   end record;
   --  To build a stack of response file names

   procedure Free is new Ada.Unchecked_Deallocation (File_Rec, File_Ptr);

   type Argument_List_Access is access Argument_List;
   procedure Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);
   --  Free only the allocated Argument_List, not allocated String components

   --------------------
   -- Arguments_From --
   --------------------

   function Arguments_From
     (Response_File_Name        : String;
      Recursive                 : Boolean := False;
      Ignore_Non_Existing_Files : Boolean := False)
      return Argument_List
   is
      First_File : File_Ptr := null;
      Last_File  : File_Ptr := null;
      --  The stack of response files

      Arguments  : Argument_List_Access := new Argument_List (1 .. 4);
      Last_Arg   : Natural := 0;

      procedure Add_Argument (Arg : String);
      --  Add argument Arg to argument list Arguments, increasing Arguments
      --  if necessary.

      procedure Recurse (File_Name : String);
      --  Get the arguments from the file and call itself recursively if one of
      --  the arguments starts with character '@'.

      ------------------
      -- Add_Argument --
      ------------------

      procedure Add_Argument (Arg : String) is
      begin
         if Last_Arg = Arguments'Last then
            declare
               New_Arguments : constant Argument_List_Access :=
                 new Argument_List (1 .. Arguments'Last * 2);
            begin
               New_Arguments (Arguments'Range) := Arguments.all;
               Arguments.all := (others => null);
               Free (Arguments);
               Arguments := New_Arguments;
            end;
         end if;

         Last_Arg := Last_Arg + 1;
         Arguments (Last_Arg) := new String'(Arg);
      end Add_Argument;

      -------------
      -- Recurse --
      -------------

      procedure Recurse (File_Name : String) is
         --  Open the response file. If not found, fail or report a warning,
         --  depending on the value of Ignore_Non_Existing_Files.

         FD : constant File_Descriptor := Open_Read (File_Name, Text);

         Buffer_Size : constant := 1500;
         Buffer : String (1 .. Buffer_Size);

         Buffer_Length : Natural;

         Buffer_Cursor : Natural;

         End_Of_File_Reached : Boolean;

         Line : String (1 .. Max_Line_Length + 1);
         Last : Natural;

         First_Char : Positive;
         --  Index of the first character of an argument in Line

         Last_Char : Natural;
         --  Index of the last character of an argument in Line

         In_String : Boolean;
         --  True when inside a quoted string

         Arg : Positive;

         function End_Of_File return Boolean;
         --  True when the end of the response file has been reached

         procedure Get_Buffer;
         --  Read one buffer from the response file

         procedure Get_Line;
         --  Get one line from the response file

         -----------------
         -- End_Of_File --
         -----------------

         function End_Of_File return Boolean is
         begin
            return End_Of_File_Reached and then Buffer_Cursor > Buffer_Length;
         end End_Of_File;

         ----------------
         -- Get_Buffer --
         ----------------

         procedure Get_Buffer is
         begin
            Buffer_Length := Read (FD, Buffer (1)'Address, Buffer'Length);
            End_Of_File_Reached := Buffer_Length < Buffer'Length;
            Buffer_Cursor := 1;
         end Get_Buffer;

         --------------
         -- Get_Line --
         --------------

         procedure Get_Line is
            Ch : Character;

         begin
            Last := 0;

            if End_Of_File then
               return;
            end if;

            loop
               Ch := Buffer (Buffer_Cursor);

               exit when Ch = ASCII.CR or else
                         Ch = ASCII.LF or else
                         Ch = ASCII.FF;

               Last := Last + 1;
               Line (Last) := Ch;

               if Last = Line'Last then
                  return;
               end if;

               Buffer_Cursor := Buffer_Cursor + 1;

               if Buffer_Cursor > Buffer_Length then
                  Get_Buffer;

                  if End_Of_File then
                     return;
                  end if;
               end if;
            end loop;

            loop
               Ch := Buffer (Buffer_Cursor);

               exit when Ch /= ASCII.HT and then
                         Ch /= ASCII.LF and then
                         Ch /= ASCII.FF;

               Buffer_Cursor := Buffer_Cursor + 1;

               if Buffer_Cursor > Buffer_Length then
                  Get_Buffer;

                  if End_Of_File then
                     return;
                  end if;
               end if;
            end loop;
         end Get_Line;

      --  Start of processing for Recurse

      begin
         Last_Arg := 0;

         if FD = Invalid_FD then
            if Ignore_Non_Existing_Files then
               return;
            else
               raise File_Does_Not_Exist;
            end if;
         end if;

         --  Put the response file name on the stack

         if First_File = null then
            First_File :=
              new File_Rec'
                (Name => new String'(File_Name),
                 Next => null,
                 Prev => null);
            Last_File  := First_File;

         else
            declare
               Current : File_Ptr := First_File;

            begin
               loop
                  if Current.Name.all = File_Name then
                     raise Circularity_Detected;
                  end if;

                  Current := Current.Next;
                  exit when Current = null;
               end loop;

               Last_File.Next :=
                 new File_Rec'
                   (Name => new String'(File_Name),
                    Next => null,
                    Prev => Last_File);
               Last_File := Last_File.Next;
            end;
         end if;

         End_Of_File_Reached := False;
         Get_Buffer;

         --  Read the response file line by line

         Line_Loop :
         while not End_Of_File loop
            Get_Line;

            if Last = Line'Last then
               raise Line_Too_Long;
            end if;

            First_Char := 1;

            --  Get each argument on the line

            Arg_Loop :
            loop
               --  First, skip any white space

               while First_Char <= Last loop
                  exit when Line (First_Char) /= ' ' and then
                            Line (First_Char) /= ASCII.HT;
                  First_Char := First_Char + 1;
               end loop;

               exit Arg_Loop when First_Char > Last;

               Last_Char := First_Char;
               In_String := False;

               --  Get the character one by one

               Character_Loop :
               while Last_Char <= Last loop

                  --  Inside a string, check only for '"'

                  if In_String then
                     if Line (Last_Char) = '"' then

                        --  Remove the '"'

                        Line (Last_Char .. Last - 1) :=
                          Line (Last_Char + 1 .. Last);
                        Last := Last - 1;

                        --  End of string is end of argument

                        if Last_Char > Last or else
                          Line (Last_Char) = ' ' or else
                          Line (Last_Char) = ASCII.HT
                        then
                           In_String := False;

                           Last_Char := Last_Char - 1;
                           exit Character_Loop;

                        else
                           --  If there are two consecutive '"', the quoted
                           --  string is not closed

                           In_String := Line (Last_Char) = '"';

                           if In_String then
                              Last_Char := Last_Char + 1;
                           end if;
                        end if;

                     else
                        Last_Char := Last_Char + 1;
                     end if;

                  elsif Last_Char = Last then

                     --  An opening '"' at the end of the line is an error

                     if Line (Last) = '"' then
                        raise No_Closing_Quote;

                     else
                        --  The argument ends with the line

                        exit Character_Loop;
                     end if;

                  elsif Line (Last_Char) = '"' then

                     --  Entering a quoted string: remove the '"'

                     In_String := True;
                     Line (Last_Char .. Last - 1) :=
                       Line (Last_Char + 1 .. Last);
                     Last := Last - 1;

                  else
                     --  Outside quoted strings, white space ends the argument

                     exit Character_Loop
                          when Line (Last_Char + 1) = ' ' or else
                               Line (Last_Char + 1) = ASCII.HT;

                     Last_Char := Last_Char + 1;
                  end if;
               end loop Character_Loop;

               --  It is an error to not close a quoted string before the end
               --  of the line.

               if In_String then
                  raise No_Closing_Quote;
               end if;

               --  Add the argument to the list

               declare
                  Arg : String (1 .. Last_Char - First_Char + 1);
               begin
                  Arg := Line (First_Char .. Last_Char);
                  Add_Argument (Arg);
               end;

               --  Next argument, if line is not finished

               First_Char := Last_Char + 1;
            end loop Arg_Loop;
         end loop Line_Loop;

         Close (FD);

         --  If Recursive is True, check for any argument starting with '@'

         if Recursive then
            Arg := 1;
            while Arg <= Last_Arg loop

               if Arguments (Arg)'Length > 0 and then
                  Arguments (Arg) (1) = '@'
               then
                  --  Ignore argument '@' with no file name

                  if Arguments (Arg)'Length = 1 then
                     Arguments (Arg .. Last_Arg - 1) :=
                       Arguments (Arg + 1 .. Last_Arg);
                     Last_Arg := Last_Arg - 1;

                  else
                     --  Save the current arguments and get those in the new
                     --  response file.

                     declare
                        Inc_File_Name     : constant String :=
                          Arguments (Arg) (2 .. Arguments (Arg)'Last);
                        Current_Arguments : constant Argument_List :=
                          Arguments (1 .. Last_Arg);
                     begin
                        Recurse (Inc_File_Name);

                        --  Insert the new arguments where the new response
                        --  file was imported.

                        declare
                           New_Arguments : constant Argument_List :=
                             Arguments (1 .. Last_Arg);
                           New_Last_Arg  : constant Positive :=
                             Current_Arguments'Length +
                             New_Arguments'Length - 1;

                        begin
                           --  Grow Arguments if it is not large enough

                           if Arguments'Last < New_Last_Arg then
                              Last_Arg := Arguments'Last;
                              Free (Arguments);

                              while Last_Arg < New_Last_Arg loop
                                 Last_Arg := Last_Arg * 2;
                              end loop;

                              Arguments := new Argument_List (1 .. Last_Arg);
                           end if;

                           Last_Arg := New_Last_Arg;

                           Arguments (1 .. Last_Arg) :=
                             Current_Arguments (1 .. Arg - 1) &
                           New_Arguments &
                           Current_Arguments
                             (Arg + 1 .. Current_Arguments'Last);

                           Arg := Arg + New_Arguments'Length;
                        end;
                     end;
                  end if;

               else
                  Arg := Arg + 1;
               end if;
            end loop;
         end if;

         --  Remove the response file name from the stack

         if First_File = Last_File then
            System.Strings.Free (First_File.Name);
            Free (First_File);
            First_File := null;
            Last_File := null;

         else
            System.Strings.Free (Last_File.Name);
            Last_File := Last_File.Prev;
            Free (Last_File.Next);
         end if;

      exception
         when others =>
            Close (FD);

            raise;
      end Recurse;

   --  Start of processing for Arguments_From

   begin
      --  The job is done by procedure Recurse

      Recurse (Response_File_Name);

      --  Free Arguments before returning the result

      declare
         Result : constant Argument_List := Arguments (1 .. Last_Arg);
      begin
         Free (Arguments);
         return Result;
      end;

   exception
      when others =>

         --  When an exception occurs, deallocate everything

         Free (Arguments);

         while First_File /= null loop
            Last_File := First_File.Next;
            System.Strings.Free (First_File.Name);
            Free (First_File);
            First_File := Last_File;
         end loop;

         raise;
   end Arguments_From;

end System.Response_File;
