-- REPORT.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  Equal to those held by the Government.
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
-- PURPOSE:
--      This Report package provides the mechanism for reporting the
--      Pass/Fail/Not-Applicable results of executable (Classes A, C,
--      D, E, and L) tests.

--      It also provides the mechanism for guaranteeing that certain
--      values become dynamic (not known at compile-time).

--      This version can write an event trace file in the format required
--      by the ACATS grading tools. Writing the event trace file requires
--      that text files can be created and appended to. If that is not true
--      for your target, set Generate_Event_Trace_File to False.

-- CHANGE HISTORY:
--      DCB 04/27/80
--      JRK 6/10/80
--      JRK 11/12/80
--      JRK 8/6/81
--      JRK 10/27/82
--      JRK 6/1/84
--      JRK 11/18/85  Added pragma Elaborate.
--      PWB 07/29/87  Added Status Action_Required and
--                    procedure Special_Action.
--      TBN 08/20/87  Added function Legal_File_Name.
--      BCB 05/17/90  Modified to allow output to Direct_IO file.
--                    Added Time-Stamp.
--      LDC 05/17/90  Removed output to Direct_IO file.
--      WMC 08/11/92  Updated ACVC version string to "9X BASIC".
--      DTN 07/05/92  Updated ACVC version string to
--                    "ACVC 2.0 JULY 6 1993 DRAFT".
--      WMC 01/24/94  Modified Legal_File_Name to allow five possible
--                    file names (increated range of type file_name to 1..5).
--      WMC 11/06/94  Updated ACVC version string to
--                    "ACVC 2.0 NOVEMBER 6 1994 DRAFT".
--      DTN 12/04/94  Updated ACVC version string to
--                    "ACVC 2.0".
--      KAS 06/19/95  Added function Ident_Wide_Char.
--      KAS 06/19/95  Added function Ident_Wide_Str.
--      DTN 11/21/95  Updated ACVC version string to "ACVC 2.0.1".
--      DTN 12/14/95  Updated ACVC version string to "ACVC 2.1".
--      EDS 12/17/97  Updated ACVC version string to "2.2".
--      RLB  3/16/00  Updated ACATS version string to "2.3".
--                    Changed various strings to read "ACATS".
--      RLB  3/22/01  Updated ACATS version string to "2.4".
--      RLB  3/29/02  Updated ACATS version string to "2.5".
--      RLB  3/06/07  Updated ACATS version string to "2.6".
--      RLB  3/22/07  Updated ACATS version string to "3.0".
--      RLB  1/24/14  Updated ACATS version string to "3.1".
--      RLB  2/28/14  Updated ACATS version string to "4.0".
--      RLB  2/29/16  Added optional .CSV event trace file.
--                    Converted file to modern format, uses Ada 2012 unit
--                    names. Note: The messages were left in all UPPER CASE
--                    in order to avoid breaking existing tools.
--      RLB  3/24/16  Moved the event trace Boolean to the specification
--                    so it can be used by a future CZ test.
--      RLB  4/27/16  Updated ACATS version string to "4.1".
--      RLB 12/29/21  Moved the ACATS version string to a separate package,
--                    found in Version.A.
--

package Report is

     subtype File_Num is Integer range 1..5;

  -- The Report routines.

     procedure Test           -- This routine must be invoked at the start of
                              -- a test, before any of the other report
                              -- routines are invoked. It save the test
                              -- name and outputs the name and description.
        ( Name : String;      -- Test Name, e.g., "C23001A-AB".
          Descr : String      -- Brief description og test, e.g.,
                              -- "Upper/lower case equivalence in " &
                              -- "identifiers".
        );

     procedure Failed         -- Output a Failure message. Should be invoked
                              -- separately to report the failure of each
                              -- subtest within a test.
        ( Descr : String      -- Brief description of what failed.
                              -- Should be phrased as:
                              -- "(Failed because) ...reason...".
        );

     procedure Not_Applicable -- Output a Not-Applicable message.
                              -- Should be invoked separately to report the
                              -- non-applicability of each subtest within a
                              -- test.
        ( Descr : String      -- Brief description of what is
                              -- Not-Applicable. Should be phrased as:
                              -- "(Not-Applicable because)...reason...".
        );

     procedure Special_Action -- Output a message describing special actions to
                              -- be taken. Should be invoked separately to give
                              -- each special action.
        ( Descr : String      -- Brief description of action to be taken.
        );

     procedure Comment        -- Output a Comment message.
        ( Descr : String      -- The message.
        );

     procedure Result;        -- This routine must be invoked at the end of a
                              -- test. It outputs a message indicating whether
                              -- the test as a whole has passed, failed, is
                              -- Not-Applicable, or has tentatively passed
                              -- pending special actions.

  -- The dynamic value routines.

     -- Even with static arguments, these functions will have dynamic
     -- results.

     function Ident_Int       -- An identity function for type Integer.
        ( X : Integer         -- The argument.
        ) return Integer;     -- X.

     function Ident_Char      -- An identity function for type
                              -- Character.
        ( X : Character       -- The argument.
        ) return Character;   -- X.

     function Ident_Wide_Char -- An identity function for type
                              -- Wide_Character.
        ( X : Wide_Character  -- The argument.
        ) return Wide_Character; -- X.

     function Ident_Bool      -- An identity function for type Boolean.
        ( X : Boolean         -- The argument.
        ) return Boolean;     -- X.

     function Ident_Str       -- An identity function for type String.
        ( X : String          -- The argument.
        ) return String;      -- X.

     function Ident_Wide_Str  -- An identity function for type Wide_String.
        ( X : Wide_String     -- The argument.
        ) return Wide_String; -- X.

     function Equal           -- A recursive Equality function for type
                              -- Integer.
        ( X, Y : Integer      -- The arguments.
        ) return Boolean;     -- X = Y.

-- Other utility routines.

     function Legal_File_Name -- A function to generate legal external file
                              -- names.
        ( X : File_Num := 1;  -- Determines first character of name.
          Nam : String := ""  -- Determines rest of name.
        ) return String;      -- The generated name.

     function Time_Stamp      -- A function to generate the time and date to
                              -- place in the output of an ACATS test.
          return String;      -- The time and date.

-- Event trace setting.

     Generate_Event_Trace_File :
                          constant Boolean := False;
          --                                  ^^^^^ --- MODIFY HERE AS NEEDED
          -- Set to True to generate an event trace file (use with an
          -- implementation that provides a compilation option to produce
          -- an event trace file), and False to not generate such a file.

end Report;


with Ada.Text_IO, Ada.Calendar, Version;
use Ada.Text_IO, Ada.Calendar;
pragma Elaborate (Ada.Text_IO, Ada.Calendar);

package body Report is

     type Status is (Pass, Fail, Does_Not_Apply, Action_Required,
                     Unknown);

     type Time_Integer is range 0 .. 86_400;

     Test_Status : Status := Fail;

     Max_Name_Len : constant := 15;     -- Maximum Test Name Length.
     Test_Name : String (1..Max_Name_Len);

     No_Name : constant String := "NO_NAME";
     Test_Name_Len : Integer range 0..Max_Name_Len := 0;

     Event_Trace_File_Name : constant String := "etrace.csv";
          -- This name will work on Windows and POSIX targets.
          -- Change it if it doesn't work.
     Max_Event_Message_Len : constant := 150;
          -- Limit event messages to this length.

     procedure Put_Msg (Msg : String) is
          -- Write message.  Long messages are folded (and indented).
          Max_Len : constant Integer range 50..150 := 72;  -- Maximum output
                                        -- line length.
          Indent : constant Integer := Test_Name_Len + 9;  -- Amount to
                                        -- indent continuation lines.
          I : Integer := 0;             -- Current Indentation.
          M : Integer := Msg'First;     -- Start of message slice.
          N : Integer;                  -- End of message slice.
     begin
          loop
               if I + (Msg'Last-M+1) > Max_Len then
                    N := M + (Max_Len-I) - 1;
                    if Msg (N) /= ' ' then
                         while N >= M and then Msg (N+1) /= ' ' loop
                              N := N - 1;
                         end loop;
                         if N < M then
                              N := M + (Max_Len-I) - 1;
                         end if;
                    end if;
               else N := Msg'Last;
               end if;
               Set_Col (Standard_Output, Ada.Text_IO.Count (I+1));
               Put_Line (Standard_Output, Msg (M..N));
               I := Indent;
               M := N + 1;
               while M <= Msg'Last and then Msg (M) = ' ' loop
                    M := M + 1;
               end loop;
               exit when M > Msg'Last;
          end loop;
     end Put_Msg;

     function Inner_Time_Stamp (Full : in Boolean := False) return String is
          Time_Now : Ada.Calendar.Time;
          Dur_Second : Day_Duration;
          Year,
          Month,
          Day,
          Hour,
          Minute,
          Second,
          SubSecond : Time_Integer := 1;

          function Convert (Number : Time_Integer) return String is
               Str : String (1..2) := (others => '0');
               Dec_Digit : constant String := "0123456789";
               Num : Time_Integer := Number;
          begin
               if Num = 0 then
                    return Str;
               else
                    Num := Num mod 100;
                    Str (2) := Dec_Digit (Integer (Num mod 10 + 1));
                    Num := Num / 10;
                    Str (1) := Dec_Digit (Integer (Num + 1));
                    return Str;
               end if;
          end Convert;
     begin
          Time_Now := Ada.Calendar.Clock;
          -- Note: This entire routine except the above call could
          -- be replaced by a call to Ada.Calendar.Formatting.Image
          -- if we don't mind the 4 digit year, and that we couldn't
          -- use the result on an Ada 95 compiler.
          Split (Time_Now, Year_Number (Year), Month_Number (Month),
                  Day_Number (Day), Dur_Second);
          if Full then
               -- Remove the fractional part without overflow (do this first
               -- so we don't round the seconds, and don't need a larger
               -- integer type [24-bits is not enough]):
               Second := Time_Integer'Max(Time_Integer(Dur_Second)-1, 0);
               Minute := Second / 60;
               -- Leave just seconds:
               Dur_Second := Dur_Second - Integer(Minute)*Day_Duration'(60.0);
               -- Multiply by 100:
               Dur_Second := Dur_Second * 100;
               SubSecond := Time_Integer(Dur_Second);
               SubSecond := SubSecond mod 100;
               -- Remove the fractional part:
               if Dur_Second <= Day_Duration(SubSecond) then
                    -- The value is just the subseconds.
                    Dur_Second := 0.0;
               else
                    Dur_Second := Dur_Second - Day_Duration(SubSecond);
               end if;
               -- Put everything back the way it was:
               Dur_Second := Dur_Second / 100;
               Dur_Second := Dur_Second + Integer(Minute)*Day_Duration'(60.0);
          end if;
          Second := Time_Integer(Dur_Second);
          Hour := Second / 3600;
          Second := Second mod 3600;
          Minute := Second / 60;
          Second := Second mod 60;
          if Full then
               return (Convert (Time_Integer (Year)/100) &
                       Convert (Time_Integer (Year)) & "-" &
                       Convert (Time_Integer (Month)) & "-" &
                       Convert (Time_Integer (Day)) & " " &
                       Convert (Time_Integer (Hour)) & ":" &
                       Convert (Time_Integer (Minute)) & ":" &
                       Convert (Time_Integer (Second)) & '.' &
                       Convert (Time_Integer (SubSecond)));
          else
               return (Convert (Time_Integer (Year)) & "-" &
                       Convert (Time_Integer (Month)) & "-" &
                       Convert (Time_Integer (Day)) & " " &
                       Convert (Time_Integer (Hour)) & ":" &
                       Convert (Time_Integer (Minute)) & ":" &
                       Convert (Time_Integer (Second)));
          end if;
     end Inner_Time_Stamp;

     function Time_Stamp return String is
     begin
          return Inner_Time_Stamp;
     end Time_Stamp;

     procedure Put_Event_Trace (Event : in String;
                                Msg : in String) is
          -- Put an event trace.
          File : Ada.Text_IO.File_Type;
     begin
          if not Generate_Event_Trace_File then
               return; -- Nothing to do, shouldn't get here.
          end if;
          begin
               Ada.Text_IO.Open (File, Name => Event_Trace_File_Name,
                    Mode => Ada.Text_IO.Append_File);
          exception
               when Ada.Text_IO.Name_Error =>
                    -- The file doesn't exist, create it.
                    Ada.Text_IO.Create (File, Name => Event_Trace_File_Name,
                        Mode => Ada.Text_IO.Out_File);
                    -- Write a header. This is not strictly necessary,
                    -- but we do it as it makes it a lot easier to use
                    -- the .CSV file in an spreadsheet.
                    Ada.Text_IO.Put_Line (File,
                                          "Event,Timestamp,Name," &
                                          "Line,Position,Message");
          end;
          Ada.Text_IO.Put (File, Event & ",""" &
                           Inner_Time_Stamp(Full => True) & """,""" &
                           Test_Name (1..Test_Name_Len) & """,,,""");
          declare
               Msg_Copy : String := Msg;
          begin
               for I in Msg_Copy'range loop
                    if Msg_Copy(I) = '"' then -- Can't have any double
                                                -- quotes in this string.
                           Msg_Copy(I) := ''';
                    end if;
               end loop;
               if Msg_Copy'Length <= Max_Event_Message_Len then
                    Ada.Text_IO.Put_Line (File, Msg_Copy & """");
               else
                    -- Find a space near the end:
                    for I in reverse Msg_Copy'First +
                       Max_Event_Message_Len-20 .. Msg_Copy'First +
                       Natural'Min(Max_Event_Message_Len+1,Msg_Copy'Length-1)
                          loop
                       if Msg_Copy(I) = ' ' then
                          Ada.Text_IO.Put_Line (File,
                             Msg_Copy (Msg_Copy'First .. I - 1) & """");
                          Ada.Text_IO.Close (File);
                          return; -- Done.
                       -- else continue looking.
                       end if;
                    end loop;
                    -- No space obvious, so truncate it.
                    Ada.Text_IO.Put_Line (File,
                        Msg_Copy (Msg_Copy'First ..
                          Msg_Copy'First + Natural'Min(
                            Max_Event_Message_Len+1, Msg_Copy'Length)) & """");
               end if;
          end;
          Ada.Text_IO.Close (File);
     end Put_Event_Trace;

     procedure Test (Name : String; Descr : String) is
     begin
          Test_Status := Pass;
          if Name'Length <= Max_Name_Len then
               Test_Name_Len := Name'Length;
          else Test_Name_Len := Max_Name_Len;
          end if;
          Test_Name (1..Test_Name_Len) :=
                    Name (Name'First .. Name'First+Test_Name_Len-1);

          Put_Msg ("");
          Put_Msg (",.,. " & Test_Name (1..Test_Name_Len) & " " &
                   "ACATS " & Version.ACATS_Version & " " & Time_Stamp);
          Put_Msg ("---- " & Test_Name (1..Test_Name_Len) & " " &
                   Descr & ".");
          if Generate_Event_Trace_File then
               Put_Event_Trace ("EXSTART", Descr);
          end if;
     end Test;

     procedure Comment (Descr : String) is
     begin
          Put_Msg ("   - " & Test_Name (1..Test_Name_Len) & " " &
                   Descr & ".");
     end Comment;

     procedure Failed (Descr : String) is
     begin
          Test_Status := Fail;
          Put_Msg ("   * " & Test_Name (1..Test_Name_Len) & " " &
                   Descr & ".");
          if Generate_Event_Trace_File then
               Put_Event_Trace ("EXFAIL", Descr);
          end if;
     end Failed;

     procedure Not_Applicable (Descr : String) is
     begin
          if Test_Status = Pass or Test_Status = Action_Required then
               Test_Status := Does_Not_Apply;
          end if;
          Put_Msg ("   + " & Test_Name (1..Test_Name_Len) & " " &
                   Descr & ".");
          if Generate_Event_Trace_File then
               Put_Event_Trace ("EXNA", Descr);
          end if;
     end Not_Applicable;

     procedure Special_Action (Descr : String) is
     begin
          if Test_Status = Pass then
               Test_Status := Action_Required;
          end if;
          Put_Msg ("   ! " & Test_Name (1..Test_Name_Len) & " " &
                   Descr & ".");
          if Generate_Event_Trace_File then
               Put_Event_Trace ("EXSACT", Descr);
          end if;
     end Special_Action;

     procedure Result is
     begin
          case Test_Status is
          when Pass =>
               Put_Msg ("==== " & Test_Name (1..Test_Name_Len) &
                        " PASSED ============================.");
               if Generate_Event_Trace_File then
                    Put_Event_Trace ("EXEND", "Passed");
               end if;
          when Does_Not_Apply =>
               Put_Msg ("++++ " & Test_Name (1..Test_Name_Len) &
                        " NOT-APPLICABLE ++++++++++++++++++++.");
               if Generate_Event_Trace_File then
                    Put_Event_Trace ("EXEND", "Not-Applicable");
               end if;
          when Action_Required =>
               Put_Msg ("!!!! " & Test_Name (1..Test_Name_Len) &
                        " TENTATIVELY PASSED !!!!!!!!!!!!!!!!.");
               Put_Msg ("!!!! " & (1..Test_Name_Len => ' ') &
                        " SEE '!' COMMENTS FOR SPECIAL NOTES!!");
               if Generate_Event_Trace_File then
                    Put_Event_Trace ("EXEND", "Tentatively Passed");
               end if;
          when others =>
               Put_Msg ("**** " & Test_Name (1..Test_Name_Len) &
                        " FAILED ****************************.");
               if Generate_Event_Trace_File then
                    Put_Event_Trace ("EXEND", "Failed");
               end if;
          end case;
          Test_Status := Fail;
          Test_Name_Len := No_Name'Length;
          Test_Name (1..Test_Name_Len) := No_Name;
     end Result;

     function Ident_Int (X : Integer) return Integer is
     begin
          if Equal (X, X) then          -- Always equal.
               return X;                -- Always executed.
          end if;
          return 0;                     -- Never executed.
     end Ident_Int;

     function Ident_Char (X : Character) return Character is
     begin
          if Equal (Character'Pos(X), Character'Pos(X)) then  -- Always equal.
               return X;                -- Always executed.
          end if;
          return '0';                   -- Never executed.
     end Ident_Char;

     function Ident_Wide_Char (X : Wide_Character) return Wide_Character is
     begin
          if Equal (Wide_Character'Pos(X), Wide_Character'Pos(X)) then
                                        -- Always equal.
               return X;                -- Always executed.
          end if;
          return '0';                   -- Never executed.
     end Ident_Wide_Char;

     function Ident_Bool (X : Boolean) return Boolean is
     begin
          if Equal (Boolean'Pos(X), Boolean'Pos(X)) then  -- Always equal.
               return X;                -- Always executed.
          end if;
          return False;                 -- Never executed.
     end Ident_Bool;

     function Ident_Str (X : String) return String is
     begin
          if Equal (X'Length, X'Length) then  -- Always equal.
               return X;                -- Always executed.
          end if;
          return "";                    -- Never executed.
     end Ident_Str;

     function Ident_Wide_Str (X : Wide_String) return Wide_String is
     begin
          if Equal (X'Length, X'Length) then  -- Always equal.
               return X;                -- Always executed.
          end if;
          return "";                    -- Never executed.
     end Ident_Wide_Str;

     function Equal (X, Y : Integer) return Boolean is
          Rec_Limit : constant Integer range 1..100 := 3;  -- Recursion limit.
          Z : Boolean;                  -- Result.
     begin
          if X < 0 then
               if Y < 0 then
                    Z := Equal (-X, -Y);
               else Z := False;
               end if;
          elsif X > Rec_Limit then
               Z := Equal (Rec_Limit, Y-X+Rec_Limit);
          elsif X > 0 then
               Z := Equal (X-1, Y-1);
          else Z := Y = 0;
          end if;
          return Z;
     exception
          when others =>
               return X = Y;
     end Equal;

     function Legal_File_Name (X : File_Num := 1;
                               Nam : String := "")
                              return String is
          Suffix : String (2..6);
     begin
          if Nam = "" then
               Suffix := Test_Name(3..7);
          else
               Suffix := Nam(3..7);
          end if;

          case X is
               when 1 => return ('X' & Suffix);
               when 2 => return ('Y' & Suffix);
               when 3 => return ('Z' & Suffix);
               when 4 => return ('V' & Suffix);
               when 5 => return ('W' & Suffix);
          end case;
     end Legal_File_Name;

begin

     Test_Name_Len := No_Name'Length;
     Test_Name (1..Test_Name_Len) := No_Name;

end Report;
