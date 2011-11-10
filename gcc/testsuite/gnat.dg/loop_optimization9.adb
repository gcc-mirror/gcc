-- { dg-do compile }
-- { dg-options "-gnatws -O3" }
-- { dg-options "-gnatws -O3 -msse" { target i?86-*-* x86_64-*-* } }

with System.Soft_Links;

package body Loop_Optimization9 is

   package SSL renames System.Soft_Links;

   First_Temp_File_Name : constant String := "GNAT-TEMP-000000.TMP";

   Current_Temp_File_Name : String := First_Temp_File_Name;

   Temp_File_Name_Last_Digit : constant Positive :=
                                 First_Temp_File_Name'Last - 4;

   function Argument_String_To_List
     (Arg_String : String) return Argument_List_Access
   is
      Max_Args : constant Integer := Arg_String'Length;
      New_Argv : Argument_List (1 .. Max_Args);
      New_Argc : Natural := 0;
      Idx      : Integer;

   begin
      Idx := Arg_String'First;

      loop
         exit when Idx > Arg_String'Last;

         declare
            Quoted  : Boolean := False;
            Backqd  : Boolean := False;
            Old_Idx : Integer;

         begin
            Old_Idx := Idx;

            loop
               --  An unquoted space is the end of an argument

               if not (Backqd or Quoted)
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string

               elsif not (Backqd or Quoted)
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;

               --  End of a quoted string and end of an argument

               elsif (Quoted and not Backqd)
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted

               elsif Arg_String (Idx) = '\' then
                  Backqd := True;

               --  Turn off backquoting after advancing one character

               elsif Backqd then
                  Backqd := False;

               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            New_Argc := New_Argc + 1;
            New_Argv (New_Argc) :=
              new String'(Arg_String (Old_Idx .. Idx - 1));
         end;
      end loop;

      return new Argument_List'(New_Argv (1 .. New_Argc));
   end Argument_String_To_List;

   procedure Create_Temp_File_Internal
     (FD        : out File_Descriptor;
      Name      : out String_Access)
   is
      Pos      : Positive;
   begin
      File_Loop : loop
         Locked : begin
            Pos := Temp_File_Name_Last_Digit;

            Digit_Loop :
            loop
               case Current_Temp_File_Name (Pos) is
                  when '0' .. '8' =>
                     Current_Temp_File_Name (Pos) :=
                       Character'Succ (Current_Temp_File_Name (Pos));
                     exit Digit_Loop;

                  when '9' =>
                     Current_Temp_File_Name (Pos) := '0';
                     Pos := Pos - 1;

                  when others =>

                     SSL.Unlock_Task.all;
                     FD := 0;
                     Name := null;
                     exit File_Loop;
               end case;
            end loop Digit_Loop;
         end Locked;
      end loop File_Loop;
   end Create_Temp_File_Internal;

end Loop_Optimization9;
