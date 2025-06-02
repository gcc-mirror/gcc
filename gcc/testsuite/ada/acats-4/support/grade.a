-- GRADE.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- PURPOSE:
--      This tool grades ACATS test results that are in the format of
--      an event trace .CSV file. (See the ACATS documentation for
--      details).
--
--      Command line (all one line):
--         Grade <Event_Trace_File_Name> <Summary_of_Tests_File_Name>
--              <Quoted Report Title> [options]
--
--         where [options] is zero or more of (case insensitive):
--            -Specs_Optional
--                Compiling of specifications is optional. (Use for
--                source-based compilers [like GNAT] that don't compile
--                specifications in normal operation.) Compilation of bodies,
--                instances, and so on are checked.
--            -Check_All_Compiles
--                All compilations are checked and must be present (unless
--                processing the unit is marked as optional).
--            -No_Compile_Checks
--                No compilation checks are made. (This option is not allowed
--                for formal conformity assessments).
--            -Use_Time_Stamps
--                Check event trace time stamp information as part of checking,
--                specifically, enforce that all units of a test are compiled
--                and run in an appropriate order and reasonably close in time.
--            -No_Time_Stamps
--                Do not make any check of event trace time stamps. (This
--                option is not allowed for formal conformity assessments).
--            -Use_Positions
--                Use position information when checking whether errors are
--                appropriately detected.
--            -No_Positions
--                Use only line information when checking whether errors are
--                appropriately detected. Use if the event trace doesn't have
--                position information. (It is acceptable to use this option
--                for formal conformity assessments).
--            -Quiet
--                Produce minimal information: a list of failed tests and
--                the summary report.
--            -Verbose
--                Produce information about every test processed (including
--                passed tests), along with the summary report.
--                (Note: We also produce a warning for multiple messages
--                for one error tag in this mode; we might want a separate
--                option for that.)
--            -Normal
--                Produce details about each failed test, along with the
--                summary report.
--
--         Only one of -Specs_Optional, -Check_All_Compiles,
--         or -No_Compile_Checks can be given. Only one of -Use_Positions or
--         -No_Positions can be given. Only one of -Quiet, -Verbose, or
--         -Normal can be given. Only one of -Use_Time_Stamps or
--         -No_Time_Stamps can be given.
--
-- CHANGE HISTORY:
--      7 Mar 2016   RLB  Created tool.
--     14 Mar 2016   RLB  Adding more grading checks.
--     16 Mar 2016   RLB  Added a separate class for errors on OK lines.
--     16 May 2016   RLB  Added Annex_C_Requirement.
--     30 May 2016   RLB  Made pragma units optional for compilation
--                        check purposes.
--     23 Jun 2016   RLB  Fixed summary sorting bug, missing manual grading
--                        list uses.
--     24 Jun 2016   RLB  Added smarter handling of units with exactly one
--                        error.
--     27 Jan 2017   RLB  Added manual grading to missing execution.
--                        Corrected so the correct result is returned for
--                        a binder crash.
--      1 Feb 2017   RLB  Added summary totals for each class of results,
--                        and "Progress" score.
--      3 Feb 2017   RLB  Added manual grading to missing binding.
--      6 Feb 2017   RLB  Corrected the single error cases to include errors
--                        with no positions.
--      9 Feb 2017   RLB  Corrected so Annex C Rqmt are properly counted.
--     22 Dec 2017   RLB  Fixed symbol for manual grading of missing set.

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Calendar.Arithmetic, Ada.Calendar.Formatting;
with Ada.Containers.Generic_Array_Sort;
with Trace, Test_Summary, Grading_Data;
procedure Grade is

   DEBUG : constant Boolean := True;

   MAX_TEST_RUN_TIME : constant Duration := 3600.0;
      -- The maximum time that all of the operations of a test are allowed
      -- to take (from the first compilation to the end of the run).

   Kill_Tool : exception;

   type Noise_Type is (Unset, Quiet, Normal, Verbose);
   Noise_Level : Noise_Type := Unset;

   type Compile_Check_Type is (Unset, Check_None, Check_Bodies, Check_All);
   Compile_Check_Level : Compile_Check_Type := Unset;

   Use_Positions : Boolean := True;
   Positions_Explicit : Boolean := False; -- Have we seen a Position option?

   Use_Time_Stamps : Boolean := True;
   Time_Stamps_Explicit : Boolean := False;
       -- Have we seen a Time Stamp option?

   type Result_Type is (Unknown,
      Fail_Process,       -- The process for the test does not meet the
                          -- requirements: the events are out of a reasonable
                          -- order (executing before compiling, etc.),
                          -- units are compiled in an unallowed order (if
                          -- a test contains multiple source files, all of
                          -- the units from the first file [the one with
                          -- sequence 0] have to be compiled before any other
                          -- units, and so on. Units within a single source
                          -- file can be compiled in any legal order.),
                          -- a test is bound or executed multiple times,
                          -- too many units are compiled for a source file,
                          -- and so on. This usually indicates a problem with
                          -- the way the tests are run (but also can represent
                          -- a problem with the way that the event log is
                          -- created). Many of the checks that lead to this
                          -- result can be suppressed with the -No_Time_Stamps
                          -- and -No_Compile_Check options (but it's better to
                          -- fix the underlying problem).
      Fail_Compile_Missing, -- Some compilation is missing completely.
      Fail_Compile_Crash, -- Some compilation started but did not finish.
      Fail_Compile_OK_Error, -- An error in the range of an OK marker.
      Fail_Compile_Error, -- Any unexpected error (other than in the range of
                          -- an OK marker) during compilation.
                          -- For a B-Test, this is extra errors beyond the
                          -- expected ones.
      Fail_Compile_Missing_Error, -- B-Tests: Expected errors missing.
      Fail_Bind_Missing,  -- Tests other than B-Tests should be bound.
      Fail_Bind_Crash,    -- The bind started but did not finish.
      Fail_Bind_Error,    -- Tests other than L-Tests should not have
                          -- bind errors.
      Fail_Run_Missing,   -- Other than B-Tests should try to execute.
      Fail_Run_Crash,     -- The execution of a test did not finish.
      Fail_Run_Message,   -- The execution printed a failure message.
      NA_Annex_C_Error,   -- Test is Not-Applicable due to an expected
                          -- error IFF Annex C is not being tested. (Further
                          -- errors don't change this result).
      NA_Compile_Error,   -- Test is Not-Applicable due to an expected
                          -- error (further errors don't change this result).
      NA_Run_Message,     -- The execution printed a not applicable message
                          -- (but no failure message).
      SH_Run_Message,     -- The execution printed a special handling message
                          -- (but no failure nor N/A message).
      Manual_Grading,     -- The test failed because of Fail_Compile_xx_Error,
                          -- Fail_Bind_Error, or Fail_Run_Message, and it is
                          -- named in the Manual_Grade_Test file. The test will
                          -- need to be manually graded, else it is considered
                          -- failed.
      Pass_Compile_Error, -- B-Tests, L-Tests: Errors as expected (and no
                          -- unexpected errors).
      Pass_Bind_Error,    -- L-Tests: Bind errors are expected.
      Pass_Run);          -- The execution did not display any non-comment
                          -- messages, and did complete properly. *WARNING*
                          -- This grading tool cannot
                          -- detect crashes that happen after the result is
                          -- written. As noted in clause 5.6.1 of the ACATS
                          -- User's Guide, a test that does not terminate
                          -- gracefully is regarded as failed. Depending
                          -- solely upon the grading tools leaves an
                          -- implementer at risk of failing a formal
                          -- conformity assessment if a spot check turns up
                          -- any "aberrant behavior" (as described in 5.6.1).
   Total : constant Result_Type := Unknown;
      -- For the purposes of result summaries, we use Result_Type = Total
      -- to represent the total number of tests graded.

   type Summary_Count is array (Result_Type) of Natural;

   Overall_Summary : Summary_Count := (others => 0);

   B_Test_Summary : Summary_Count := (others => 0);

   C_Test_Summary : Summary_Count := (others => 0);

   L_Test_Summary : Summary_Count := (others => 0);

   Other_Test_Summary : Summary_Count := (others => 0);
       -- Class A, D, E.

   package Int_IO is new Ada.Text_IO.Integer_IO (Natural);

   procedure Process_Option (Opt : in String) is
      -- Process a single option.
      Upper_Opt : constant String :=
         Ada.Strings.Fixed.Translate (Opt,
            Ada.Strings.Maps.Constants.Upper_Case_Map);
   begin
      if Upper_Opt = "-SPECS_OPTIONAL" then
         if Compile_Check_Level = Unset then
            Compile_Check_Level := Check_Bodies;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple compilation check options"
               & " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-CHECK_ALL_COMPILES" then
         if Compile_Check_Level = Unset then
            Compile_Check_Level := Check_All;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple compilation check options"
               & " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-NO_COMPILE_CHECKS" then
         if Compile_Check_Level = Unset then
            Compile_Check_Level := Check_None;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple compilation check options"
               & " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-USE_TIME_STAMPS" then
         if not Time_Stamps_Explicit then
            Use_Time_Stamps := True;
            Time_Stamps_Explicit := True;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple time stamp options" &
               " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-NO_TIME_STAMPS" then
         if not Time_Stamps_Explicit then
            Use_Time_Stamps := False;
            Time_Stamps_Explicit := True;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple time stamp options" &
               " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-USE_POSITIONS" then
         if not Positions_Explicit then
            Use_Positions := True;
            Positions_Explicit := True;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple position options" &
               " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-NO_POSITIONS" then
         if not Positions_Explicit then
            Use_Positions := False;
            Positions_Explicit := True;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple position options" &
               " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-QUIET" then
         if Noise_Level = Unset then
            Noise_Level := Quiet;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple verbosity options" &
               " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-NORMAL" then
         if Noise_Level = Unset then
            Noise_Level := Normal;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple verbosity options" &
               " given on command line");
            raise Kill_Tool;
         end if;
      elsif Upper_Opt = "-VERBOSE" then
         if Noise_Level = Unset then
            Noise_Level := Verbose;
         else
            Ada.Text_IO.Put_Line ("*ERROR* Multiple verbosity options" &
               " given on command line");
            raise Kill_Tool;
         end if;
      else
         Ada.Text_IO.Put_Line ("*ERROR* Option " & Opt & " is not" &
            " recognized");
         raise Kill_Tool;
      end if;
   end Process_Option;


   function Grade_Test (First_Item,
                        Last_Item : in Grading_Data.Summary_Item_Count;
                        First_Event, Last_Event : in Grading_Data.Event_Count)
      return Result_Type is
      -- Grade a single test; the summary items for the test (all source
      -- files) are found between First_Item and Last_Item.
      -- Similarly, the events (if any) for the test are found between
      -- First_Event and Last_Event.
      use type Grading_Data.Event_Count;        -- For operators.
      use type Grading_Data.Summary_Item_Count; -- For operators.
      use type Trace.Event_Type;                -- For operators.

      -- A special case:
      L_Test_Had_Expected_Compile_Time_Error : Boolean := False;


      type Message_Kind is (Passed, Failed, NA, Special_Action, Warning);
      procedure Message_For_Event (Kind : in Message_Kind;
                                   Item : in Grading_Data.Summary_Item_Count;
                                   Event: in Grading_Data.Event_Count;
                                   Text : in String) is
         -- Produce a message of the form
         -- "?? Test <test_name> <text> on line <Event Error_Line> in
         --        source file <item source file>
         --     Message: <Event Mess>
         -- This message is for some sort of error record, but it is not
         -- necessarily an error! Use Kind=Passed if the message is not
         -- an error but rather some informational message (usually for
         -- Verbose mode). The caller should determine the appropriate
         -- "Noise_Level".
         -- Do not display the message in quiet mode; do not
         -- display the message if Event = 0; do not display the line and
         -- source file is Event is not a Compile_Error or Compile_Warning
         -- event.
      begin
         case Kind is
            when Passed =>
               Ada.Text_IO.Put ("-- Test ");
            when Failed =>
               Ada.Text_IO.Put ("** Test ");
            when NA =>
               Ada.Text_IO.Put ("++ Test ");
            when Special_Action =>
               Ada.Text_IO.Put ("!! Test ");
            when Warning =>
               Ada.Text_IO.Put ("?? Warning - Test ");
          end case;
          Ada.Text_IO.Put (Grading_Data.Summary_of_Tests(Item).
                              Source_Name(1..7) & " " & Text);
          if Event /= 0 and then
           (Grading_Data.Event_Trace(Event).Event = Trace.Compile_Error or else
            Grading_Data.Event_Trace(Event).Event = Trace.Compile_Warning) then
               Ada.Text_IO.Put_Line (" at line" &
                  Trace.Line_Number_Type'
                     Image(Grading_Data.Event_Trace(Event).Error_Line) &
                  " in file " &
                  Grading_Data.Event_Trace(Event).Name);
          elsif Event /= 0 and then
           (Grading_Data.Event_Trace(Event).Event = Trace.Compilation_Start
             or else
            Grading_Data.Event_Trace(Event).Event = Trace.Compilation_End) then
               Ada.Text_IO.Put_Line (" of file " &
                  Grading_Data.Event_Trace(Event).Name);
           else
               Ada.Text_IO.New_Line;
           end if;
           if Noise_Level /= Quiet and then
              Event /= 0 and then
              Ada.Strings.Unbounded.Length (
                 Grading_Data.Event_Trace(Event).Message) /= 0 then
                 Ada.Text_IO.Put_Line ("   because: " &
                    Ada.Strings.Unbounded.To_String(
                       Grading_Data.Event_Trace(Event).Message));
           end if;
      end Message_For_Event;

   begin
      -- Basic grading rules:

      -- Summary files are created by our tools, and can be recreated as
      -- needed, so we consider them trustworthy and don't make checks
      -- specifically on them. In contrast, we do check that event trace
      -- files meet basic requirements (in an appropriate temporal order,
      -- that errors belong to a compilation, etc.).

      -- When a check fails, we can (and do) immediately return that fact -
      -- further checks are pointless. (We do not try to report *all*
      -- reasons for failure, just the first.) However, for other results,
      -- we have to continue to check (at least for a while). We don't
      -- make all possible checks in some cases (such as Not Applicable tests)
      -- as those may reject acceptable behavior. In unlikely cases, we might
      -- miss errors this way, but we don't want to let the code devolve to
      -- an inpenatrable mess of flags and special cases.

      -- The failure reason that we find is what is reported. We try to make
      -- the checks in the order of most interesting to least interesting.
      -- Process checks are made early (if the process -- that is, the order
      -- of operations) is wrong, any other results are dubious at best. Then
      -- checks for compile crashes are made, then followed by those
      -- involving errors are made, followed by checks that everything
      -- needed is compiled. Then we make binding checks, and lastly execution
      -- checks.

      -- For each compilation, binding, and execution, we require a matching
      -- end. If these are missing, we grade the test as "crashed". This
      -- might represent an unexpected and thus unhandled exception, some
      -- sort of internal error (like a GNAT bug box), or a problem with the
      -- event trace.

      -- Note that these tools cannot find all possible failures (especially
      -- with test execution). As noted in clause 5.6.1 of the ACATS
      -- User's Guide, a test that does not terminate gracefully is regarded
      -- as failed, even if it prints "PASSED". Similarly, it's possible
      -- for a compiler or bind to report that it has successfully finished,
      -- and still fail to exit cleanly. In all of these cases, the event
      -- trace will contain a normal "end" record, and thus the tool will not
      -- be able to see the failure.
      --
      -- *WARNING* As a test is failed if it does not exit cleanly, depending
      -- solely upon the grading tools leaves an implementer at risk of failing
      -- a formal conformity assessment if a spot check turns up any "aberrant
      -- behavior" for an executable test (as described in 5.6.1).

      if Noise_Level = Verbose then
         Ada.Text_IO.Put_Line ("Grading Test " &
            Grading_Data.Summary_of_Tests(First_Item).Source_Name(1..7));
         Ada.Text_IO.Put_Line ("  Items:" &
            Grading_Data.Summary_Item_Count'Image (First_Item) & " .." &
            Grading_Data.Summary_Item_Count'Image (Last_Item) & "; " &
            "  Events:" &
            Grading_Data.Event_Count'Image (First_Event) & " .." &
            Grading_Data.Event_Count'Image (Last_Event));
      -- else no output.
      end if;

      if Use_Time_Stamps then
         declare
            use type Ada.Calendar.Time;
         begin
            -- Our first check is that all of the time stamps of the events
            -- are close enough together.
            --
            -- Since the events of a test are sorted in time-stamp order,
            -- we can just check the first and last times (assuming that
            -- there any events).
            if First_Event <= Last_Event and then
               Grading_Data.Event_Trace(First_Event).Timestamp +
                  MAX_TEST_RUN_TIME <
                     Grading_Data.Event_Trace(Last_Event).Timestamp then
               Message_For_Event (Kind => Failed,
                                  Item => First_Item,
                                  Event=> 0, -- No event here.
                                  Text => "took too long complete");
               if Noise_Level /= Quiet then
                  Ada.Text_IO.Put_Line ("   Time of first event=" &
                     Ada.Calendar.Formatting.Image (
                        Grading_Data.Event_Trace(First_Event).Timestamp,
                        Include_Time_Fraction => True) &
                     "; time of last event=" &
                     Ada.Calendar.Formatting.Image (
                        Grading_Data.Event_Trace(Last_Event).Timestamp,
                        Include_Time_Fraction => True));
                  begin
                     Ada.Text_IO.Put_Line ("   Elapsed time=" &
                        Ada.Calendar.Formatting.Image (
                           Grading_Data.Event_Trace(Last_Event).Timestamp -
                           Grading_Data.Event_Trace(First_Event).Timestamp,
                           Include_Time_Fraction => True));
                  exception
                     when Ada.Calendar.Time_Error =>
                        null; -- Too long, forget it.
                  end;
               end if;
               return Fail_Process;
            -- else OK.
            end if;

            -- The second check is that, the time stamps of all compilations
            -- are in ascending order for units in different files (the
            -- sequence numbers must be obeyed) - order doesn't matter for
            -- files in the same source file; binding (if any) must happen
            -- after all compiles, and execution (if any) must happen after
            -- all binding.

            for Ev in First_Event .. Last_Event - 1 loop
               if Grading_Data.Event_Trace(Ev).Event in Trace.Compile_Event
                  then
                  -- All following compile events with a larger source name
                  -- (that is, a larger sequence number), and all binding
                  -- and execution events have to have the same or later
                  -- time stamp.
                  for I in Ev+1 .. Last_Event loop
                     if Grading_Data.Event_Trace(I).Event in
                        Trace.Compile_Event then
                        if Grading_Data.Event_Trace(Ev).Timestamp >
                           Grading_Data.Event_Trace(I).Timestamp and then
                           Grading_Data.Event_Trace(Ev).Name <
                           Grading_Data.Event_Trace(I).Name then
                            Message_For_Event (Kind => Failed,
                                               Item => First_Item,
                                               Event=> Ev,
                                               Text =>
                                  "failed by compiling " &
                                  Grading_Data.Event_Trace(I).Name &
                                  " before compiliation");
                            if Noise_Level = Verbose then
                               Ada.Text_IO.Put_Line ("   Binding=" &
                                  Grading_Data.Event_Count'Image (I) &
                                  "; Compilation=" &
                                  Grading_Data.Event_Count'Image (Ev));
                            end if;
                           return Fail_Process;
                        -- else OK.
                        end if;
                     elsif Grading_Data.Event_Trace(I).Event in
                        Trace.Binder_Event then
                        if Grading_Data.Event_Trace(Ev).Timestamp >
                           Grading_Data.Event_Trace(I).Timestamp then
                            Message_For_Event (Kind => Failed,
                                               Item => First_Item,
                                               Event=> Ev,
                                               Text =>
                                  "failed by binding before compilation");
                            if Noise_Level = Verbose then
                               Ada.Text_IO.Put_Line ("   Binding=" &
                                  Grading_Data.Event_Count'Image (I) &
                                  "; Compilation=" &
                                  Grading_Data.Event_Count'Image (Ev));
                            end if;
                           return Fail_Process;
                        -- else OK.
                        end if;
                     elsif Grading_Data.Event_Trace(I).Event in
                        Trace.Execution_Event then
                        if Grading_Data.Event_Trace(Ev).Timestamp >
                           Grading_Data.Event_Trace(I).Timestamp then
                            Message_For_Event (Kind => Failed,
                                               Item => First_Item,
                                               Event=> Ev,
                                               Text =>
                                  "failed by executing before compilation");
                            if Noise_Level = Verbose then
                               Ada.Text_IO.Put_Line ("   Execution=" &
                                  Grading_Data.Event_Count'Image (I) &
                                  "; Compilation=" &
                                  Grading_Data.Event_Count'Image (Ev));
                            end if;
                           return Fail_Process;
                        -- else OK.
                        end if;
                     -- else no other events.
                     end if;
                  end loop;

               elsif Grading_Data.Event_Trace(Ev).Event in
                           Trace.Binder_Event then
                  -- All execution events have to have the same or later
                  -- time stamp.
                  for I in Ev+1 .. Last_Event loop
                     if Grading_Data.Event_Trace(I).Event in
                        Trace.Execution_Event then
                        if Grading_Data.Event_Trace(Ev).Timestamp >
                           Grading_Data.Event_Trace(I).Timestamp then
                            Message_For_Event (Kind => Failed,
                                               Item => First_Item,
                                               Event=> Ev,
                                               Text =>
                                  "failed by executing before binding");
                            if Noise_Level = Verbose then
                               Ada.Text_IO.Put_Line ("   Execution=" &
                                  Grading_Data.Event_Count'Image (I) &
                                  "; Binding=" &
                                  Grading_Data.Event_Count'Image (Ev));
                            end if;
                           return Fail_Process;
                        -- else OK.
                        end if;
                     -- else no checked needed for events.
                     end if;
                  end loop;

               elsif Grading_Data.Event_Trace(Ev).Event in
                  Trace.Execution_Event then
                  null; -- No check needed here.

               -- else no other events.
               end if;
            end loop;

            -- We could make additional checks.
            -- In particular, we could check that for each
            -- compilation, ends come after starts, and all other events
            -- come between the first start and the last end, and similarly
            -- for binder and execution events.
            --
            -- Because the sorting of items with identical timestamps is
            -- imperfect, we have to be fairly careful with such checks.
            -- As such, we've left these checks out for now, they would only
            -- to fail in cases of very buggy event trace generators or
            -- fraudulent event traces, neither of which are very likely.
         end;
      end if;

      -- Next, compile completion checking.
      --
      -- For each source file, we check that if there are
      -- N Compilation_Start records, then there must be N Compilation_End
      -- records. More or less is a failure (Fail_Compile_Crash for less,
      -- Fail_Process for more).
      -- Less usually represents a crashed compilation (like a GNAT bug
      -- box, a hang that was terminated, or a Janus/Ada exception walkback).
      -- (More suggests that there is a problem with the event trace.)

      -- Note: This is a sloppy version of this check, in that the check is
      -- repeated N*2 times for a test file with N compilation units. There
      -- aren't enough units nor does this check take enough time to make it
      -- worthwhile to figure out a less duplicative check.
      --
      -- Note that we have to check both Compilation_Start and Compilation_End
      -- events in case one or the other is missing.
      --
      -- We also check that all other compilation events have a matching start.

      for Ev in First_Event .. Last_Event loop
          if Grading_Data.Event_Trace(Ev).Event = Trace.Compilation_Start
             or else
             Grading_Data.Event_Trace(Ev).Event = Trace.Compilation_End then
             declare
                Count : Integer := 0;
             begin
                for I in First_Event .. Last_Event loop
                   if Grading_Data.Event_Trace(I).Event =
                      Trace.Compilation_Start then
                      Count := Count + 1;
                   elsif Grading_Data.Event_Trace(I).Event =
                      Trace.Compilation_End then
                      Count := Count - 1;
                   -- else not interested.
                   end if;
                end loop;
                if Count > 0 then
                   -- More starts than ends:
                   Message_For_Event (Kind => Failed,
                                      Item => First_Item,
                                      Event=> Ev,
                                      Text =>
                                  "crash in compilation");
                   return Fail_Compile_Crash;
                elsif Count < 0 then
                   -- More ends that starts:
                   Message_For_Event (Kind => Failed,
                                      Item => First_Item,
                                      Event=> Ev,
                                      Text =>
                                  "more compilation ends than starts");
                   return Fail_Process;
                -- else OK.
                end if;
             end;
         elsif Grading_Data.Event_Trace(Ev).Event in
            Trace.Compile_Event then
            -- Ensure that there is at least one Compilation_Start for the
            -- appropriate source file.
            for I in First_Event .. Last_Event loop
               if Grading_Data.Event_Trace(I).Event =
                  Trace.Compilation_Start and then
                  Grading_Data.Event_Trace(Ev).Name =
                  Grading_Data.Event_Trace(I).Name then
                  goto Comp_OK;
               -- else not a match.
               end if;
            end loop;
            -- If we get here, we never found a match.
            Message_For_Event (Kind => Failed,
                               Item => First_Item,
                               Event=> Ev,
                               Text =>
               "no compilation start found for event");
                   return Fail_Process;
           <<Comp_OK>> null;
         -- else not interested in other events.
         end if;
      end loop;

      -- Next, check errors. (This is the main event, after all; it's hard
      -- to check errors for B-Tests.)

      -- We check all of the errors (if any) listed in the test summary.
      -- For each matching event, we mark it as "expected". That will make
      -- it easy to find "unexpected" errors when we're done.

      -- The range of an error depends on the Use_Positions flag: we check
      -- only the line numbers if it is False, otherwise, the location of the
      -- error has to be within the indicated start and end positions as well.
      -- There is an exception for a compilation unit with exactly one error
      -- or optional error: the location of an error doesn't matter (so long
      -- as it is in the correct source file).

      -- For an Error tag, some error must be reported in the range of the
      -- error tag; else the test is failed. A warning is generated in
      -- Verbose mode if there is more than one error in the appropriate range.

      -- For an Optional Error:
      -- (1) For an L-Test, an error reported in the range of the error
      -- tag means the test has passed; no error is OK but doesn't change the
      -- test status. In both cases, other units have to be checked.
      -- (2) For a B-Test, any error reported in the range of the error
      -- has no effect on the test status.
      -- (3) For other tests, any error reported in the range of the error
      -- has no effect on the test status (and this is weird).

      -- Possible Error tags are graded like an Error tag, with the exception
      -- that an error need only be reported at one of the locations of a set;
      -- no warning is generated for an error at both locations. (Of course,
      -- if there is no error at either location, the test is failed.)

      -- Any error reported in the range of an OK tag means the test is failed.

      -- An error reported in range of a N/A => ERROR tag means that the test
      -- is compile-time not-applicable. We have a special case for this tag:
      -- any unexpected errors after the N/A error (including in following
      -- source files) do not make the test failed. (See the next item.)
      -- (We can handle this by marking all following errors as expected.)
      -- We also do not require marked errors after an N/A => ERROR tag;
      -- the fact that the test is N/A means that those are irrelevant.

      -- ANX C RQMT is handled the same way as an N/A => ERROR.

      -- Any error reported elsewhere ("unexpected errors") in a compilation
      -- means that the test is failed. (Assuming that the special cases
      -- noted above are not treated as "unexpected").

      declare
         Expected : array (First_Event .. Last_Event) of Boolean :=
            (others => False);
            -- Is the associated event known to be expected??
         use type Test_Summary.Info_Kind_Type;

         NA_From_Expected_Compile_Time_Error : Boolean := False;
         NA_From_Annex_C_Requirement : Boolean := False;

         type Set_Info is record
            Label : Test_Summary.PE_Label := (others => ' ');
            Saw_Error : Boolean := False;
         end record;

         Sets : array (1 .. 50) of Set_Info;

         procedure Figure_Range_for_Item (
            Tag_Item       : in  Grading_Data.Summary_Item_Index;
            Start_Line     : out Trace.Line_Number_Type;
            Start_Position : out Trace.Line_Position_Type;
            End_Line       : out Trace.Line_Number_Type;
            End_Position   : out Trace.Line_Position_Type;
            Single_Tag_in_Unit : out Boolean) is
            -- Determine the effective range for Tag_Item. If the item is
            -- the only significant item for the current compilation unit,
            -- then we use an expanded range (see below) and set
            -- Single_Tag_in_Unit. Otherwise, we just use the item's actual
            -- range.
            --
            -- We expand the range as a unit that contains only a single
            -- error case needs only to be rejected; no reason or position
            -- is necessary. OTOH, when there are multiple errors, error
            -- positions (but not message text) figure into the grading.
            Unit_First_Item,
            Unit_Last_Item,
            Unit_Item,
            Next_Unit_Item : Grading_Data.Summary_Item_Count;
            One_Unit_in_File : Boolean := True;

            function All_Possible_Errors_of_Same_Set return Boolean is
               -- Returns True if all of the tags in this unit are
               -- Possible_Errors of a single set.
            begin
               if Grading_Data.Summary_of_Tests(Tag_Item).Kind /=
                        Test_Summary.Possible_Error then
                  return False;
               end if;
               for Itm in Unit_Item + 1 .. Next_Unit_Item - 1 loop
                  -- Loop over all of the tags in the unit.
                  if Grading_Data.Summary_of_Tests(Itm).Kind /=
                        Test_Summary.Possible_Error then
                     return False;
                  end if;
                  if Grading_Data.Summary_of_Tests(Tag_Item).Set_Label /=
                     Grading_Data.Summary_of_Tests(Itm).Set_Label then
                     return False;
                  end if;
               end loop;
               return True; -- If we get here, no problems found.
            end All_Possible_Errors_of_Same_Set;

            function One_Tag_other_than_Optional_Errors return Boolean is
               -- Returns True if all of the tags in this unit are
               -- Optional_Errors, except for one that is an Error or
               -- Possible_Error.
               Saw_Good_Tag : Boolean := False;
            begin
               for Itm in Unit_Item + 1 .. Next_Unit_Item - 1 loop
                  -- Loop over all of the tags in the unit.
                  if Grading_Data.Summary_of_Tests(Itm).Kind =
                        Test_Summary.Optional_Error then
                     null; -- OK.
                  elsif Grading_Data.Summary_of_Tests(Itm).Kind =
                        Test_Summary.Error or else
                        Grading_Data.Summary_of_Tests(Itm).Kind =
                        Test_Summary.Possible_Error then
                     if Saw_Good_Tag then
                        return False; -- Two non-optional_error tags.
                     else
                        Saw_Good_Tag := True;
                     end if;
                  else
                     return False; -- Don't deal with other kinds of tags here.
                  end if;
               end loop;
               return Saw_Good_Tag; -- If we didn't see any good tag (that is,
                  -- this is only optional error tags), then forget this.
            end One_Tag_other_than_Optional_Errors;

            function Other_Leading_Tags_are_OK return Boolean is
               -- Returns True if all of the tags in this unit preceding
               -- the current one are OK. (The current tag must be last
               -- in this case.)
            begin
               if Tag_Item /= Next_Unit_Item - 1 then
                  return False;
               end if;
               for Itm in Unit_Item + 1 .. Tag_Item - 1 loop
                  -- Loop over all of the leading tags in the unit.
                  if Grading_Data.Summary_of_Tests(Itm).Kind /=
                        Test_Summary.OK then
                     return False;
                  -- else continue.
                  end if;
               end loop;
               return True; -- If we get here, no problems found.
            end Other_Leading_Tags_are_OK;

            function Other_Trailing_Tags_are_OK return Boolean is
               -- Returns True if all of the tags in this unit following
               -- the current one are OK. (The current tag must be first
               -- in this case.)
            begin
               if Tag_Item /= Unit_Item + 1 then
                  return False;
               end if;
               for Itm in Tag_Item + 1 .. Next_Unit_Item - 1 loop
                  -- Loop over all of the leading tags in the unit.
                  if Grading_Data.Summary_of_Tests(Itm).Kind /=
                        Test_Summary.OK then
                     return False;
                  -- else continue.
                  end if;
               end loop;
               return True; -- If we get here, no problems found.
            end Other_Trailing_Tags_are_OK;

         begin
            -- Figure out the range of items for this file.
            Unit_First_Item := Tag_Item;
            Unit_Last_Item := Tag_Item;
            Unit_Item := Grading_Data.Summary_Item_Count'First;
            Next_Unit_Item := Grading_Data.Summary_Item_Count'First;
            for Itm in First_Item .. Last_Item loop
               if Grading_Data.Summary_of_Tests(Itm).Source_Name =
                  Grading_Data.Summary_of_Tests(Tag_Item).Source_Name then
                  -- Belongs to the correct source file.
                  if Unit_First_Item > Itm then
                     Unit_First_Item := Itm;
                  end if;
                  if Unit_Last_Item < Itm then
                     Unit_Last_Item := Itm;
                  end if;
                  if Grading_Data.Summary_of_Tests(Itm).Kind =
                        Test_Summary.Compilation_Unit then
                     if Unit_Item = Grading_Data.Summary_Item_Count'First then
                        -- First compilation unit in the file.
                        Unit_Item := Itm;
                     elsif Itm < Tag_Item then
                        -- A unit closer to the tag (the unit should precede
                        -- the tag).
                        Unit_Item := Itm;
                        One_Unit_in_File := False;
                     else -- A trailing unit.
                        Next_Unit_Item := Itm;
                        One_Unit_in_File := False;
                     end if;
                  -- else some tag.
                  end if;
               -- else for some other file of this test.
               end if;
            end loop;
            -- Check that the results make sense:
            if Unit_Item = Grading_Data.Summary_Item_Count'First then
               raise Program_Error
                   with "No unit found in source file " &
                        Grading_Data.Summary_of_Tests(Tag_Item).Source_Name;
            end if;
            if One_Unit_in_File then
               if Next_Unit_Item = Grading_Data.Summary_Item_Count'First then
                  Next_Unit_Item := Unit_Last_Item + 1;
               else
                  raise Program_Error
                  with "One unit in source file " &
                        Grading_Data.Summary_of_Tests(Tag_Item).Source_Name &
                       " but some trailing unit as well";
               end if;
            else -- Multiple units.
               if Next_Unit_Item = Grading_Data.Summary_Item_Count'First then
                  -- Could be the last unit.
                  Next_Unit_Item := Unit_Last_Item + 1;
               else -- Already have a trailing unit.
                  null;
               end if;
            end if;
            if Tag_Item not in Unit_Item .. Next_Unit_Item - 1 and then
               Grading_Data.Summary_of_Tests(Tag_Item).Kind /=
                        Test_Summary.OK then
               raise Program_Error
                  with "Tag not in the range for this unit, in source file " &
                        Grading_Data.Summary_of_Tests(Tag_Item).Source_Name;
               -- Sorting failure?? Or a junk tag picked up in a file header.
               -- We skip OK tags as they don't use the order of tags at all
               -- (see below) and they have occurred in tests (BC3502A).
            end if;

            -- If we get here, the interesting unit and its tags are found
            -- in the range Unit_Item .. Next_Unit_Item - 1. (The tags have
            -- to follow the unit record after sorting.)
            if Grading_Data.Summary_of_Tests(Tag_Item).Kind =
                        Test_Summary.OK then
               -- We never expand the range of OK tags (that would be making
               -- grading stricter, not easier).
               Start_Line     := Grading_Data.
                                 Summary_of_Tests(Tag_Item).Start_Line;
               Start_Position := Grading_Data.
                                 Summary_of_Tests(Tag_Item).Start_Position;
               End_Line       := Grading_Data.
                                 Summary_of_Tests(Tag_Item).End_Line;
               End_Position   := Grading_Data.
                                 Summary_of_Tests(Tag_Item).End_Position;
               Single_Tag_in_Unit := False;

            elsif Tag_Item = Next_Unit_Item - 1 and then
               Unit_Item + 1 = Tag_Item and then
               One_Unit_in_File then
               -- This is the only tag in this compilation unit, and this
               -- compilation unit is the only one in this file. In this case,
               -- we use the range of file (assuming an extra line at the end
               -- of the unit).
               Start_Line     := 0;
               Start_Position := 0; -- Include any errors that are missing
                                    -- positional information.
               End_Line       := Grading_Data.
                                 Summary_of_Tests(Unit_Item).End_Line + 2;
               End_Position   := Trace.Line_Position_Type'Last;
               Single_Tag_in_Unit := True;
               if DEBUG and Noise_Level = Verbose then
                  Ada.Text_IO.Put_Line ("== Single error in " &
                     "single unit for file " &
                     Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
               end if;

            elsif Tag_Item = Next_Unit_Item - 1 and then
               Unit_Item + 1 = Tag_Item then
               -- This is the only tag in this compilation unit, but there are
               -- multiple units in this file. In this case,
               -- we use the range of the unit.
               Start_Line     := Grading_Data.
                                 Summary_of_Tests(Unit_Item).Start_Line;
               Start_Position := Grading_Data.
                                 Summary_of_Tests(Unit_Item).Start_Position;
               End_Line       := Grading_Data.
                                 Summary_of_Tests(Unit_Item).End_Line;
               End_Position   := Grading_Data.
                                 Summary_of_Tests(Unit_Item).End_Position;
               Single_Tag_in_Unit := True;
               if DEBUG and Noise_Level = Verbose then
                  Ada.Text_IO.Put_Line ("== Single error in " &
                     "a unit for multiple unit file " &
                     Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
               end if;

            elsif All_Possible_Errors_of_Same_Set then
               -- All of the tags in this compilation unit are Possible_Errors
               -- for a single set. These act like a single tag (see previous
               -- two cases).
               if One_Unit_in_File then
                  Start_Line     := 0;
                  Start_Position := 0;
                  End_Line       := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Line + 1;
                  End_Position   := Trace.Line_Position_Type'Last;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single possible error set in " &
                        "single unit for file " &
                        Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               else
                  Start_Line     := Grading_Data.
                                    Summary_of_Tests(Unit_Item).Start_Line;
                  Start_Position := Grading_Data.
                                    Summary_of_Tests(Unit_Item).Start_Position;
                  End_Line       := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Line;
                  End_Position   := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Position;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single possible error set in " &
                        "a unit for multiple unit file " &
                       Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               end if;

            elsif Grading_Data.Summary_of_Tests(Tag_Item).Kind /=
               Test_Summary.Optional_Error and then
               One_Tag_other_than_Optional_Errors then
               -- All of the other tags are optional errors. Combined with an
               -- single Error or Possible_Error, these can be ignored and
               -- we can treat the current tag as if it is alone (see
               -- the second and third cases for why we use these ranges).
               if One_Unit_in_File then
                  Start_Line     := 0;
                  Start_Position := 0;
                  End_Line       := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Line + 1;
                  End_Position   := Trace.Line_Position_Type'Last;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single error with optional " &
                        "errors in single unit for file " &
                        Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               else
                  Start_Line     := Grading_Data.
                                    Summary_of_Tests(Unit_Item).Start_Line;
                  Start_Position := Grading_Data.
                                    Summary_of_Tests(Unit_Item).Start_Position;
                  End_Line       := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Line;
                  End_Position   := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Position;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single error with optional " &
                        "errors in a unit for multiple unit file " &
                       Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               end if;

            elsif Grading_Data.Summary_of_Tests(Tag_Item).Kind =
               Test_Summary.Optional_Error and then
               One_Tag_other_than_Optional_Errors then
               -- This is an optional error that is subsumed by the range of
               -- the one "good" tag. Give it a null range to avoid
               -- spurious overlap errors.
               Start_Line     := Grading_Data.
                                 Summary_of_Tests(Unit_Item).Start_Line;
               Start_Position := Trace.Line_Position_Type'Last;
               End_Line       := Grading_Data.
                                 Summary_of_Tests(Unit_Item).Start_Line;
               End_Position   := 1;
               Single_Tag_in_Unit := True;
               if DEBUG and Noise_Level = Verbose then
                  Ada.Text_IO.Put_Line ("== Optional error of a single " &
                        "error in file " &
                       Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
               end if;

            elsif Other_Leading_Tags_are_OK then
               -- All of the other tags are OK tags, and they precede this tag.
               -- We treat the current tag as if it is alone, with the
               -- difference that we use the last OK tag to mark the start of
               -- errors (there can't be any errors where an OK tag is found).
               Start_Line     := Grading_Data.
                                 Summary_of_Tests(Tag_Item-1).End_Line;
               Start_Position := Grading_Data.
                                 Summary_of_Tests(Tag_Item-1).End_Position;
               if Start_Position /= Trace.Line_Position_Type'Last then
                  Start_Position := Start_Position + 1;
               end if;
               if One_Unit_in_File then
                  End_Line       := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Line + 1;
                  End_Position   := Trace.Line_Position_Type'Last;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single error with leading OK " &
                        "tags in single unit for file " &
                        Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               else
                  End_Line       := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Line;
                  End_Position   := Grading_Data.
                                    Summary_of_Tests(Unit_Item).End_Position;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single error with leading OK " &
                        "tags in a unit for multiple unit file " &
                       Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               end if;

            elsif Other_Trailing_Tags_are_OK then
               -- All of the other tags are OK tags, and they follow this tag.
               -- We treat the current tag as if it is alone, with the
               -- difference that we use the first OK tag to mark the end of
               -- errors (there can't be any errors where an OK tag is found).
               End_Line     := Grading_Data.
                               Summary_of_Tests(Tag_Item+1).Start_Line;
               End_Position := Grading_Data.
                               Summary_of_Tests(Tag_Item+1).Start_Position;
               if End_Position /= Trace.Line_Position_Type'First then
                  End_Position := End_Position - 1;
               end if;
               if One_Unit_in_File then
                  Start_Line     := 0;
                  Start_Position := 0;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single error with trailing OK "&
                        "tags in single unit for file " &
                        Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               else
                  Start_Line     := Grading_Data.
                                    Summary_of_Tests(Unit_Item).Start_Line;
                  Start_Position := Grading_Data.
                                    Summary_of_Tests(Unit_Item).Start_Position;
                  Single_Tag_in_Unit := True;
                  if DEBUG and Noise_Level = Verbose then
                     Ada.Text_IO.Put_Line ("== Single error with trailing OK "&
                        "tags in a unit for multiple unit file " &
                       Grading_Data.Summary_of_Tests(Tag_Item).Source_Name);
                  end if;
               end if;

            -- %%% Additional special cases could be added here, such as
            -- mixed OKs, and so on. We'll do that if those cases
            -- actually appear in practice and they would help.

            else
               -- None of the special cases above. Just use the range of the
               -- tag.
               Start_Line     := Grading_Data.
                                 Summary_of_Tests(Tag_Item).Start_Line;
               Start_Position := Grading_Data.
                                 Summary_of_Tests(Tag_Item).Start_Position;
               End_Line       := Grading_Data.
                                 Summary_of_Tests(Tag_Item).End_Line;
               End_Position   := Grading_Data.
                                 Summary_of_Tests(Tag_Item).End_Position;
               Single_Tag_in_Unit := False;
            end if;
         end Figure_Range_for_Item;


      begin
         for Sum in First_Item .. Last_Item loop
            if Grading_Data.Summary_of_Tests(Sum).Kind in
               Test_Summary.Error .. Test_Summary.OK then
               -- An interesting "tag" in a test file.
               declare
                  Start_Line     : Trace.Line_Number_Type;
                  Start_Position : Trace.Line_Position_Type;
                  End_Line       : Trace.Line_Number_Type;
                  End_Position   : Trace.Line_Position_Type;
                  Saw_One        : Boolean := False;
                  Single_Tag_in_Unit : Boolean := False;
               begin
                  Figure_Range_for_Item (Sum, Start_Line, Start_Position,
                     End_Line, End_Position, Single_Tag_in_Unit);

                  if not Use_Positions then
                     Start_Position := 0;
                     End_Position   := Trace.Line_Position_Type'Last;
                  end if;
                  -- Check for all matching compile_error events.
                  -- An event matches if it has the right source name,
                  -- and it has the correct range.
                  for Ev in First_Event .. Last_Event loop
                     if Grading_Data.Event_Trace(Ev).Event =
                        Trace.Compile_Error and then
                        Grading_Data.Event_Trace(Ev).Name =
                        Grading_Data.Summary_of_Tests(Sum).Source_Name and then
                        (Grading_Data.Event_Trace(Ev).Error_Line in
                              Start_Line + 1 .. End_Line - 1 or else
                           (Grading_Data.Event_Trace(Ev).Error_Line =
                              Start_Line and then
                            Grading_Data.Event_Trace(Ev).Error_Position >=
                              Start_Position) or else
                           (Grading_Data.Event_Trace(Ev).Error_Line =
                              End_Line and then
                            Grading_Data.Event_Trace(Ev).Error_Position <=
                              End_Position)) then
                        -- A match!
                        if Expected(Ev) then
                           -- Previously marked. Some sort of overlap if there
                           -- are multiple tags in the unit; if there is only
                           -- one, however, the whole unit is the range and
                           -- later messages probably don't overlap anything.
                           -- Additionally, if the unit is N/A, we don't
                           -- want any messages about following errors (they're
                           -- irrelevant).
                           if (not Single_Tag_in_Unit) and then
                              (not NA_From_Expected_Compile_Time_Error)and then
                              (not NA_From_Annex_C_Requirement) then
                              Message_For_Event (Kind => Warning,
                                                 Item => Sum,
                                                 Event=> Ev,
                                                 Text =>
                                     "appears to contain an error overlap");
                              if Noise_Level /= Quiet then
                                 Ada.Text_IO.Put_Line ("  Affected event:");
                                 Grading_Data.Put_Event_Line (Ev);
                                 Ada.Text_IO.Put_Line ("  New item:");
                                 Grading_Data.Put_Summary_Item_Line (Sum);
                              end if;
                           -- else -- the entire unit is the range, or the
                           --      -- test has already been determined to be NA
                           end if;
                        elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                           Test_Summary.Error then
                           Expected(Ev) := True;
                           if Saw_One then
                              if Noise_Level = Verbose then
                                Message_For_Event (Kind => Warning,
                                                   Item => Sum,
                                                   Event=> Ev,
                                                   Text =>
                                         "has more than one error for " &
                                         "an expected error");
                              end if;
                           else
                              Saw_One := True;
                              if Noise_Level = Verbose then
                                 Message_For_Event (Kind => Passed,
                                    Item => Sum,
                                    Event=> Ev,
                                    Text => "has expected error");
                              end if;
                           end if;
                        elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                           Test_Summary.OK then
                           if Grading_Data.Manual_Grading_Requested_for_Test (
                                 Grading_Data.Summary_of_Tests(First_Item).
                                     Source_Name) then
                              Message_For_Event (Kind => Special_Action,
                                                 Item => Sum,
                                                 Event=> Ev,
                                                 Text =>
                                 "has an error for an OK item, " &
                                 "manual grading requested");
                              return Manual_Grading;
                           else
                              Message_For_Event (Kind => Failed,
                                                 Item => Sum,
                                                 Event=> Ev,
                                                 Text =>
                                 "failed by having an error for an OK item");
                              return Fail_Compile_OK_Error;
                           end if;
                        elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                           Test_Summary.Optional_Error then
                           Expected(Ev) := True;
                           Saw_One := True; -- Saw an error for this tag.
                           if Grading_Data.Summary_of_Tests(First_Item).
                              Source_Name(1) /= 'L' then
                              if Noise_Level = Verbose then
                                 Message_For_Event (Kind => Passed,
                                                    Item => Sum,
                                                    Event=> Ev,
                                                    Text =>
                                              "ignoring optional error");
                              end if;
                           else
                              L_Test_Had_Expected_Compile_Time_Error := True;
                              if Noise_Level /= Quiet then
                                 Message_For_Event (Kind => Passed,
                                                    Item => Sum,
                                                    Event=> Ev,
                                                    Text =>
                                           "has expected optional error");
                              end if;
                           end if;
                        elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                           Test_Summary.NA_Error then
                           Expected(Ev) := True;
                           NA_From_Expected_Compile_Time_Error := True;
                           if Noise_Level /= Quiet then
                              Message_For_Event (Kind => NA,
                                                 Item => Sum,
                                                 Event=> Ev,
                                                 Text =>
                                              "N/A because of expected error");
                           end if;
                           Saw_One := True; -- Saw an error for this tag.
                           -- We allow any extra errors following this one,
                           -- as error cascades are likely. Set all following
                           -- events to expected:
                           for I in Ev+1 .. Last_Event loop
                              Expected(I) := True;
                           end loop;

                        elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                           Test_Summary.Annex_C_Requirement then
                           Expected(Ev) := True;
                           NA_From_Annex_C_Requirement := True;
                           if Noise_Level /= Quiet then
                              Message_For_Event (Kind => NA,
                                                 Item => Sum,
                                                 Event=> Ev,
                                                 Text =>
                                        "NA (if Annex C not being tested) " &
                                        "because of expected error");
                           end if;
                           Saw_One := True; -- Saw an error for this tag.
                           -- We allow any extra errors following this one,
                           -- as error cascades are likely. Set all following
                           -- events to expected:
                           for I in Ev+1 .. Last_Event loop
                              Expected(I) := True;
                           end loop;

                        elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                           Test_Summary.Possible_Error then
                           Expected(Ev) := True;
                           declare
                              Set_Loc : Natural := 0;
                           begin
                              -- Find/set the location of the set in our
                              -- set data:
                              for I in Sets'range loop
                                 if Sets(I).Label =
                                    Grading_Data.Summary_of_Tests(Sum).
                                       Set_Label then
                                    Set_Loc := I;
                                    exit;
                                 elsif Sets(I).Label =
                                    Test_Summary.PE_Label'(others => ' ') then
                                    -- Empty slot, use it.
                                    Sets(I) := (Label =>
                                       Grading_Data.Summary_of_Tests(Sum).
                                          Set_Label,
                                       Saw_Error => False);
                                    Set_Loc := I;
                                    exit;
                                 -- else a used slot, not the right set.
                                 end if;
                              end loop;
                              if Set_Loc = 0 then
                                 raise Program_Error
                                    with " not enough Possible Error slots!!!";
                              end if;
                              if Sets(Set_Loc).Saw_Error then
                                 if Noise_Level = Verbose then
                                    Message_For_Event (Kind => Warning,
                                                       Item => Sum,
                                                       Event=> Ev,
                                                       Text =>
                                             "has more than one error for " &
                                             "an expected possible error");
                                 end if;
                              else
                                 Sets(Set_Loc).Saw_Error := True;
                                 if Noise_Level = Verbose then
                                    Message_For_Event (Kind => Passed,
                                        Item => Sum,
                                        Event=> Ev,
                                        Text => "has expected possible error");
                                 end if;
                              end if;
                           end;
                        -- else not some sort of test tag.
                        end if;
                     elsif Debug and then Noise_Level = Verbose and then
                        Grading_Data.Event_Trace(Ev).Event =
                        Trace.Compile_Error and then
                        Grading_Data.Event_Trace(Ev).Name =
                        Grading_Data.Summary_of_Tests(Sum).Source_Name then
                        -- Some compiler error item that is out of range.
                        if Single_Tag_in_Unit then
                            -- Should not be able to get here.
                            Message_For_Event (Kind => Warning,
                                               Item => Sum,
                                               Event=> Ev,
                                               Text =>
                                             "skipped error item in " &
                                             "single error case");
                            --Ada.Text_IO.Put ("Start_Line=" &
                            --   Trace.Line_Number_Type'Image(Start_Line));
                            --Ada.Text_IO.Put ("; Start_Position=" &
                            -- Trace.Line_Position_Type'Image(Start_Position));
                            --Ada.Text_IO.Put ("; End_Line=" &
                            --   Trace.Line_Number_Type'Image(End_Line));
                            --Ada.Text_IO.Put ("; End_Position=" &
                            --   Trace.Line_Position_Type'Image(End_Position));
                            --Ada.Text_IO.New_Line;
                            --Ada.Text_IO.Put ("In range=" &
                            --  Boolean'Image(
                            --   (Grading_Data.Event_Trace(Ev).Error_Line in
                            --      Start_Line + 1 .. End_Line - 1 or else
                            --   (Grading_Data.Event_Trace(Ev).Error_Line =
                            --      Start_Line and then
                            --   Grading_Data.Event_Trace(Ev).Error_Position >=
                            --     Start_Position) or else
                            --   (Grading_Data.Event_Trace(Ev).Error_Line =
                            --      End_Line and then
                            --   Grading_Data.Event_Trace(Ev).Error_Position <=
                            --      End_Position))));
                            --Ada.Text_IO.New_Line;
                        -- else no trace yet.
                        end if;
                     -- else not a matching Compile_Error event.
                     end if;
                  end loop;

                  -- Figure any additional results.
                  if Grading_Data.Summary_of_Tests(Sum).Kind =
                     Test_Summary.Error then
                     if not Saw_One then -- No error for this expected error.
                        if NA_From_Expected_Compile_Time_Error or else
                           NA_From_Annex_C_Requirement then
                           -- No requirement for errors in an N/A test.
                           if DEBUG and then Noise_Level = Verbose then
                               Message_For_Event (Kind => Passed,
                                                  Item => Sum,
                                                  Event=> 0, -- No event.
                                                  Text =>
                               "no error OK for an ERROR item in an NA test" &
                               " at line" &
                               Trace.Line_Number_Type'Image(Grading_Data.
                                   Summary_of_Tests(Sum).End_Line) &
                               " in file " &
                               Grading_Data.Summary_of_Tests(Sum).Source_Name);
                            -- else no message wanted.
                            end if;
                        elsif Grading_Data.Manual_Grading_Requested_for_Test (
                              Grading_Data.Summary_of_Tests(Sum).Source_Name)
                                 then
                           Message_For_Event (Kind => Special_Action,
                                              Item => Sum,
                                              Event=> 0, -- No event.
                                              Text =>
                              "did not report an error for an ERROR item " &
                              "at line" &
                              Trace.Line_Number_Type'Image(Grading_Data.
                                   Summary_of_Tests(Sum).End_Line) &
                              " in file " &
                              Grading_Data.Summary_of_Tests(Sum).Source_Name &
                              ", manual grading requested");
                            return Manual_Grading;
                        else
                           Message_For_Event (Kind => Failed,
                                              Item => Sum,
                                              Event=> 0, -- No event.
                                              Text =>
                              "failed by not reporting an " &
                              "error for an ERROR item at line" &
                              Trace.Line_Number_Type'Image(Grading_Data.
                                   Summary_of_Tests(Sum).End_Line) &
                              " in file " &
                              Grading_Data.Summary_of_Tests(Sum).Source_Name);
                            return Fail_Compile_Missing_Error;
                        end if;
                     end if;
                  elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                        Test_Summary.Possible_Error then
                     declare
                        Set_Loc : Natural := 0;
                     begin
                        -- Find/set the location of the set in our set data:
                        for I in Sets'range loop
                           if Sets(I).Label =
                              Grading_Data.Summary_of_Tests(Sum).Set_Label then
                              Set_Loc := I;
                              exit;
                           elsif Sets(I).Label =
                              Test_Summary.PE_Label'(others => ' ') then
                              -- Empty slot, use it.
                              Sets(I) := (Label =>
                                 Grading_Data.Summary_of_Tests(Sum).Set_Label,
                                 Saw_Error => False);
                              Set_Loc := I;
                              exit;
                           -- else a used slot, not the right set.
                           end if;
                        end loop;
                        if Set_Loc = 0 then
                           raise Program_Error
                              with " not enough Possible Error slots!!!";
                        end if;
                        if Sets(Set_Loc).Saw_Error then
                           null; -- We saw an error previously, so we don't
                                 -- need one here.
                           if DEBUG and then Noise_Level = Verbose then
                              Message_For_Event (Kind => Passed,
                                                 Item => Sum,
                                                 Event=> 0, -- No event.
                                                 Text =>
                              "no error OK because of previous error for " &
                              "a POSSIBLE ERROR item of set " &
                              Grading_Data.Summary_of_Tests(Sum).Set_Label &
                              " at line" &
                              Trace.Line_Number_Type'Image(Grading_Data.
                                   Summary_of_Tests(Sum).End_Line) &
                              " in file " &
                              Grading_Data.Summary_of_Tests(Sum).Source_Name);
                           end if;
                        else
                           -- We'll assume the labels are unique within a test
                           -- (that's the rule, hut we've never really depended
                           -- upon it before).
                           -- If this is the last Possible Error for this set
                           -- in the summary file, we have to report a missing
                           -- error. Otherwise, we'll just expect the error
                           -- to be reported later.
                           for I in Sum+1 .. Last_Item loop
                              if Grading_Data.Summary_of_Tests(I).Kind =
                                 Test_Summary.Possible_Error and then
                                 Grading_Data.Summary_of_Tests(Sum).Set_Label =
                                 Grading_Data.Summary_of_Tests(I).Set_Label
                                 then
                                 goto PE_OK; -- Another Possible Error later
                                             -- in the test.
                              -- else not an appropriate Possible_Error.
                              end if;
                           end loop;
                           -- If we get here, we're the last Possible_Error,
                           -- and we never saw an error at any of them. Barf.
                           if NA_From_Expected_Compile_Time_Error or else
                              NA_From_Annex_C_Requirement then
                              -- No requirement for errors in an N/A test.
                               if DEBUG and then Noise_Level = Verbose then
                                   Message_For_Event (Kind => Passed,
                                                      Item => Sum,
                                                     Event=> 0, -- No event.
                                                     Text =>
                                  "no error OK for a POSSIBLE ERROR item in" &
                                  " an NA test at line" &
                                  Trace.Line_Number_Type'Image(Grading_Data.
                                       Summary_of_Tests(Sum).End_Line) &
                                  " in file " & Grading_Data.
                                           Summary_of_Tests(Sum).Source_Name);
                               -- else no message wanted.
                               end if;
                           elsif
                              Grading_Data.Manual_Grading_Requested_for_Test (
                                Grading_Data.Summary_of_Tests(Sum).Source_Name)
                                    then
                              Message_For_Event (Kind => Special_Action,
                                                 Item => Sum,
                                                 Event=> 0, -- No event.
                                                 Text =>
                                 "did not report an " &
                                 "error for any POSSIBLE ERROR items of set " &
                                 Grading_Data.Summary_of_Tests(Sum).Set_Label &
                                 " at line" &
                                 Trace.Line_Number_Type'Image(Grading_Data.
                                      Summary_of_Tests(Sum).End_Line) &
                                 " in file " &
                                 Grading_Data.Summary_of_Tests(Sum).Source_Name
                                    & ", manual grading requested");
                              return Manual_Grading;
                           else
                              Message_For_Event (Kind => Failed,
                                                 Item => Sum,
                                                 Event=> 0, -- No event.
                                                 Text =>
                                 "failed by not reporting an " &
                                 "error for any POSSIBLE ERROR items of set " &
                                 Grading_Data.Summary_of_Tests(Sum).Set_Label &
                                 " at line" &
                                 Trace.Line_Number_Type'Image(Grading_Data.
                                      Summary_of_Tests(Sum).End_Line) &
                                 " in file " &
                                 Grading_Data.Summary_of_Tests(Sum).
                                    Source_Name);
                              return Fail_Compile_Missing_Error;
                           end if;
                           <<PE_OK>> null;
                        end if;
                     end;
                  elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                        Test_Summary.Optional_Error then
                     if DEBUG and then (not Saw_One) and then
                        Noise_Level = Verbose then
                        Message_For_Event (Kind => Passed,
                                           Item => Sum,
                                           Event=> 0, -- No event.
                                           Text =>
                          "no error OK for an OPTIONAL ERROR item" &
                          " at line" &
                          Trace.Line_Number_Type'Image(Grading_Data.
                              Summary_of_Tests(Sum).End_Line) &
                          " in file " &
                          Grading_Data.Summary_of_Tests(Sum).Source_Name);
                     -- else saw a error, or no message wanted.
                     end if;
                  elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                        Test_Summary.NA_Error then
                     if DEBUG and then (not Saw_One) and then
                        Noise_Level = Verbose then
                        Message_For_Event (Kind => Passed,
                                           Item => Sum,
                                           Event=> 0, -- No event.
                                           Text =>
                          "no error OK for a N/A => ERROR item" &
                          " at line" &
                          Trace.Line_Number_Type'Image(Grading_Data.
                              Summary_of_Tests(Sum).End_Line) &
                          " in file " &
                          Grading_Data.Summary_of_Tests(Sum).Source_Name);
                     -- else saw a error, or no message wanted.
                     end if;
                  elsif Grading_Data.Summary_of_Tests(Sum).Kind =
                        Test_Summary.Annex_C_Requirement then
                     if DEBUG and then (not Saw_One) and then
                        Noise_Level = Verbose then
                        Message_For_Event (Kind => Passed,
                                           Item => Sum,
                                           Event=> 0, -- No event.
                                           Text =>
                          "no error OK for a Annex C Rqmt item" &
                          " at line" &
                          Trace.Line_Number_Type'Image(Grading_Data.
                              Summary_of_Tests(Sum).End_Line) &
                          " in file " &
                          Grading_Data.Summary_of_Tests(Sum).Source_Name);
                     -- else saw a error, or no message wanted.
                     end if;
                  end if;
               end;
            -- else not a "tag".
            end if;
         end loop;

         -- Check if there are any "unexpected" errors. If so, report
         -- the test failed.
         for Ev in Expected'Range loop
            if (not Expected(Ev)) and then
               Grading_Data.Event_Trace(Ev).Event = Trace.Compile_Error then
               if Grading_Data.Manual_Grading_Requested_for_Test (
                    Grading_Data.Summary_of_Tests(First_Item).Source_Name) then
                  Message_For_Event (Kind => Special_Action,
                                     Item => First_Item,
                                     Event=> Ev,
                                     Text =>
                     "has unexpected compile error, manual grading requested");
                  return Manual_Grading;
               else
                  Message_For_Event (Kind => Failed,
                                     Item => First_Item,
                                     Event=> Ev,
                                     Text =>
                     "has unexpected compile error");
                  return Fail_Compile_Error;
               end if;
            end if;
         end loop;

         if NA_From_Expected_Compile_Time_Error then
            -- We do this check after the previous so that any unexpected
            -- errors that precede the N/A tag get detected.
            -- Previously reported if we're going to report it.
            return NA_Compile_Error;
         end if;
         if NA_From_Annex_C_Requirement then
            -- We do this check after the previous so that any unexpected
            -- errors that precede the N/A tag get detected.
            -- Previously reported if we're going to report it.
            return NA_Annex_C_Error;
         end if;
      end;

      -- Now, compile existence checking.
      --
      -- If Compile_Check_Level is Check_All, all compilation units (other
      -- than those marked optional) must be compiled.
      -- If Compile_Check_Level is Check_Bodies, all compilation units (other
      -- than those marked optional and specifications) must be compiled.
      -- If Compile_Check_Level is Check_None, no compilations are required.
      -- But any compilations that exist still will have any rules
      -- that apply to them (like time stamps and errors) checked.
      -- Since we can't depend on line numbers in the event trace, all
      -- we can check is that the number of compilations is right for
      -- each source file. This is a range in general.
      -- Specifically: Given that the number of units in a source file is
      -- N, number of optional units is Opt, and the number of specifications
      -- S, then
      -- If Compile_Check_Level is Check_All, then N-Opt .. N compilations
      -- are needed (more or less is a failure).
      -- If Compile_Check_Level is Check_Bodies, then N-S-Opt .. N
      -- compilations are needed (more or less is a failure).
      -- If Compile_Check_Level is Check_None, then 0 .. N
      -- compilations are needed (more or less is a failure).
      -- In all cases, Less is Fail_Compile_Missing, More is Fail_Process.
      --
      -- It would be best to use the Start_Line to check that all expected
      -- units are compiled, but that's somewhat of a problem since the
      -- split between units is not well-defined (it's somewhere in the
      -- whitespace between the units). The problem is that the summary
      -- picks an exact place.
      --
      -- We treat pragma compilation units as always optional, since
      -- they might be compiled with a following unit or by some other means.

      -- Note This is sloppy version of this check, in that the check is
      -- repeated N times for a unit with N compilation units. There aren't
      -- enough units nor does this check take enough time to make it
      -- worthwhile to figure out a less duplicative check.

      declare
         Spec_Count, Body_Count, Pragma_Count,
                       Opt_Spec_Count, Opt_Body_Count : Natural;
            -- "Body" here means everything not classified as a spec.
            -- or pragma.
         Comp_Count : Natural;
         use type Test_Summary.Info_Kind_Type;
         use type Test_Summary.Compilation_Unit_Kinds;
      begin
         for Itm in First_Item .. Last_Item loop
            if Grading_Data.Summary_of_Tests(Itm).Kind =
               Test_Summary.Compilation_Unit then
               Spec_Count   := 0; Opt_Spec_Count := 0;
               Body_Count   := 0; Opt_Body_Count := 0;
               Pragma_Count := 0;
               Comp_Count   := 0;
               for Sum in First_Item .. Last_Item loop
                  if Grading_Data.Summary_of_Tests(Sum).Kind =
                     Test_Summary.Compilation_Unit and then
                     Grading_Data.Summary_of_Tests(Itm).Source_Name =
                     Grading_Data.Summary_of_Tests(Sum).Source_Name then
                     case Grading_Data.Summary_of_Tests(Sum).Unit_Kind is
                        when Test_Summary.Package_Specification |
                             Test_Summary.Procedure_Specification |
                             Test_Summary.Function_Specification |
                             Test_Summary.Generic_Package |
                             Test_Summary.Generic_Procedure |
                             Test_Summary.Generic_Function |
                             Test_Summary.Package_Renaming |
                             Test_Summary.Procedure_Renaming |
                             Test_Summary.Function_Renaming |
                             Test_Summary.Generic_Package_Renaming |
                             Test_Summary.Generic_Procedure_Renaming |
                             Test_Summary.Generic_Function_Renaming =>
                           Spec_Count := Spec_Count + 1;
                           if Grading_Data.Summary_of_Tests(Sum).Optional then
                              Opt_Spec_Count := Opt_Spec_Count + 1;
                           end if;
                        when Test_Summary.Configuration_Pragma =>
                           Pragma_Count := Pragma_Count + 1;
                        when others =>
                           Body_Count := Body_Count + 1;
                           if Grading_Data.Summary_of_Tests(Sum).Optional then
                              Opt_Body_Count := Opt_Body_Count + 1;
                           end if;
                     end case;
                  -- else not from this source file.
                  end if;
               end loop;
               if DEBUG and then Noise_Level = Verbose then
                  if Pragma_Count = 0 then
                     Ada.Text_IO.Put_Line ("For source file " &
                        Grading_Data.Summary_of_Tests(Itm).Source_Name &
                        "; expect to compile" & Natural'Image(Spec_Count) &
                        " specs with" & Natural'Image(Opt_Spec_Count) &
                        " optional; and " & Natural'Image(Body_Count) &
                        " bodies with" & Natural'Image(Opt_Body_Count) &
                        " optional");
                  else
                     Ada.Text_IO.Put_Line ("For source file " &
                        Grading_Data.Summary_of_Tests(Itm).Source_Name &
                        "; expect to compile" & Natural'Image(Pragma_Count) &
                        " pragmas;" & Natural'Image(Spec_Count) &
                        " specs with" & Natural'Image(Opt_Spec_Count) &
                        " optional; and " & Natural'Image(Body_Count) &
                        " bodies with" & Natural'Image(Opt_Body_Count) &
                        " optional");
                  end if;
               end if;

               for Ev in First_Event .. Last_Event loop
                  if Grading_Data.Event_Trace(Ev).Event =
                     Trace.Compilation_Start and then
                     Grading_Data.Event_Trace(Ev).Name =
                     Grading_Data.Summary_of_Tests(Itm).Source_Name then
                     Comp_Count := Comp_Count + 1;
                  -- else not a compilation of a unit from the correct
                  -- source file.
                  end if;
               end loop;

               if DEBUG and then Noise_Level = Verbose then
                  Ada.Text_IO.Put_Line ("For source file " &
                     Grading_Data.Summary_of_Tests(Itm).Source_Name &
                     "; actual compilations" & Natural'Image(Comp_Count));
               end if;

               if Comp_Count > Spec_Count + Body_Count + Pragma_Count then
                  -- More compilations than units for a source file,
                  -- always a process error.
                  Message_For_Event (Kind => Failed,
                                     Item => Itm,
                                     Event=> 0, -- No event.
                                     Text =>
                  "more compilations than units in file " &
                     Grading_Data.Summary_of_Tests(Itm).Source_Name);
                  return Fail_Process;
               end if;

               if Compile_Check_Level = Check_All then
                  if Comp_Count < Spec_Count + Body_Count -
                     Opt_Spec_Count - Opt_Body_Count then
                     -- Fewer compilations than allowed for a source file.
                     if Grading_Data.Manual_Grading_Requested_for_Test (
                           Grading_Data.Summary_of_Tests(Itm).Source_Name) then
                        -- Presumably, the test needs to be split in order for
                        -- all of the units to be processed.
                        Message_For_Event (Kind => Special_Action,
                                           Item => Itm,
                                           Event=> 0, -- No event.
                                           Text =>
                              "not all required units of file " &
                                 Grading_Data.Summary_of_Tests(Itm).Source_Name
                                 & " are compiled, manual grading requested");
                        return Manual_Grading;
                     else
                        Message_For_Event (Kind => Failed,
                                           Item => Itm,
                                           Event=> 0, -- No event.
                                           Text =>
                           "not all required units of file " &
                              Grading_Data.Summary_of_Tests(Itm).Source_Name &
                              " are compiled");
                        return Fail_Compile_Missing;
                     end if;
                  -- else OK.
                  end if;
               elsif Compile_Check_Level = Check_Bodies then
                  if Comp_Count < Body_Count - Opt_Body_Count then
                     -- Fewer compilations than allowed for a source file.
                     if Grading_Data.Manual_Grading_Requested_for_Test (
                           Grading_Data.Summary_of_Tests(Itm).Source_Name) then
                        -- Presumably, the test needs to be split in order for
                        -- all of the units to be processed.
                        Message_For_Event (Kind => Special_Action,
                                           Item => Itm,
                                           Event=> 0, -- No event.
                                           Text =>
                              "not all required units of file " &
                                 Grading_Data.Summary_of_Tests(Itm).Source_Name
                                 & " are compiled, manual grading requested");
                        return Manual_Grading;
                     else
                        Message_For_Event (Kind => Failed,
                                           Item => Itm,
                                           Event=> 0, -- No event.
                                           Text =>
                           "not all required units of file " &
                              Grading_Data.Summary_of_Tests(Itm).Source_Name &
                              " are compiled");
                        return Fail_Compile_Missing;
                     end if;
                  -- else OK.
                  end if;
               elsif Compile_Check_Level = Check_None then
                  null; -- No further check needed.
               -- else Unset should be impossible.
               end if;
           -- else not a compilation unit with a source file name.
           end if;
         end loop;
      end;


      -- All test types other than B-Tests can check binding and execution.
      -- (For a L-Test, execution of most tests will report Failed; but it
      -- is possible for a few tests to pass. Either way, we don't need to
      -- test these specially. If the compile or binding had a failure, we
      -- won't get here.)

      if Grading_Data.Summary_of_Tests(First_Item).Source_Name(1) = 'B' then
         -- If we get here, we've made all of the compile time checks, and
         -- there are no failures. So all errors must have been detected
         -- as expected.
         if Noise_Level /= Quiet then
             Message_For_Event (Kind => Passed,
                                Item => First_Item,
                                Event=> 0, -- No event.
                                Text =>
                   "passed by detecting all expected errors");
         -- else other results were already reported if they will be.
         end if;
         return Pass_Compile_Error;
      end if;

      if Grading_Data.Summary_of_Tests(First_Item).Source_Name(1) = 'L'
         and then L_Test_Had_Expected_Compile_Time_Error then
         -- If we get here, we've made all of the compile time checks, and
         -- there are no failures. So this test is passed with compile-time
         -- detection of the error.
         if Noise_Level /= Quiet then
             Message_For_Event (Kind => Passed,
                                Item => First_Item,
                                Event=> 0, -- No event.
                                Text =>
                   "passed by detecting a problem at compile-time");
         -- else other results were already reported if they will be.
         end if;
         return Pass_Compile_Error;
      end if;

      -- Binding checks:
      declare
         Start : Grading_Data.Event_Count := 0;
      begin
         -- Check that there is exactly one Binder_Start:
         for Ev in First_Event .. Last_Event loop
            if Grading_Data.Event_Trace(Ev).Event = Trace.Binder_Start then
               if Start = 0 then
                  Start := Ev;
               else
                  Message_For_Event (Kind => Failed,
                                     Item => First_Item,
                                     Event=> 0, -- No event.
                                     Text => "was bound multiple times");
                  if Noise_Level /= Quiet then
                     Ada.Text_IO.Put_Line ("  First run=" &
                        Grading_Data.Event_Count'Image (Start) &
                        "; Second run=" &
                        Grading_Data.Event_Count'Image (Ev));
                  end if;
                  return Fail_Process;
               end if;
            -- else not interesting.
            end if;
         end loop;
         if Start = 0 then
            -- No binding found.
            if Grading_Data.Manual_Grading_Requested_for_Test (
               Grading_Data.Summary_of_Tests(First_Item).Source_Name) then
               Message_For_Event (Kind => Special_Action,
                                  Item => First_Item,
                                  Event=> 0, -- No event.
                                  Text => "has no binding, manual " &
                                          "grading requested");
               return Manual_Grading;
            else
               Ada.Text_IO.Put_Line ("** Binding of Test " &
                  Grading_Data.Summary_of_Tests(First_Item).Source_Name(1..7)
                  & " is missing");
               return Fail_Bind_Missing;
            end if;
         -- else found exactly one binder start.
         end if;

         -- Check that there is exactly one Binder_End:
         Start := 0;
         for Ev in First_Event .. Last_Event loop
            if Grading_Data.Event_Trace(Ev).Event = Trace.Binder_End then
               if Start = 0 then
                  Start := Ev;
               else
                  Message_For_Event (Kind => Failed,
                                     Item => First_Item,
                                     Event=> 0, -- No event.
                                     Text =>
                         "has multiple binding completion events");
                  if Noise_Level /= Quiet then
                     Ada.Text_IO.Put_Line ("  First run=" &
                        Grading_Data.Event_Count'Image (Start) &
                        "; Second run=" &
                        Grading_Data.Event_Count'Image (Ev));
                  end if;
                  return Fail_Process;
               end if;
            -- else not interesting.
            end if;
         end loop;
         if Start = 0 then
            -- No binder completion found, assume a crash.
            Message_For_Event (Kind => Failed,
                               Item => First_Item,
                               Event=> 0, -- No event.
                               Text => "binding crashed");
            return Fail_Bind_Crash;
         -- else found exactly one binder completion.
         end if;

         -- Now, figure a result based on any binder errors. The result is
         -- pass for any L-Tests, and failure otherwise.
         for Ev in First_Event .. Last_Event loop
            if Grading_Data.Event_Trace(Ev).Event =
               Trace.Binder_Error then
               if Grading_Data.Summary_of_Tests(First_Item).Source_Name(1)
                  = 'L' then
                  if Noise_Level /= Quiet then
                     Message_For_Event (Kind => Passed,
                                        Item => First_Item,
                                        Event=> Ev,
                                        Text =>
                                        "passes because of binder error(s)");
                  end if;
                  return Pass_Bind_Error;
               else -- Other classes of tests shouldn't have binder errors.
                  if Grading_Data.Manual_Grading_Requested_for_Test (
                       Grading_Data.Summary_of_Tests(First_Item).Source_Name)
                          then
                     Message_For_Event (Kind => Special_Action,
                                        Item => First_Item,
                                        Event=> Ev,
                                        Text => "has a binder error, manual " &
                                                "grading requested");
                     return Manual_Grading;
                  else
                     Message_For_Event (Kind => Failed,
                                        Item => First_Item,
                                        Event=> Ev,
                                        Text => "has a binder error");
                     return Fail_Bind_Error;
                  end if;
               end if;
            -- else not interesting.
            end if;
         end loop;
      end;

      -- Execution checks:
      declare
         Start : Grading_Data.Event_Count := 0;
         Ex_Result : Result_Type := Pass_Run;
      begin
         -- Check that there is exactly one Execution_Start:
         for Ev in First_Event .. Last_Event loop
            if Grading_Data.Event_Trace(Ev).Event = Trace.Execution_Start then
               if Start = 0 then
                  Start := Ev;
               else
                  Message_For_Event (Kind => Failed,
                                     Item => First_Item,
                                     Event=> 0, -- No event.
                                     Text => "was run multiple times");
                  if Noise_Level /= Quiet then
                     Ada.Text_IO.Put_Line ("  First run=" &
                        Grading_Data.Event_Count'Image (Start) &
                        "; Second run=" &
                        Grading_Data.Event_Count'Image (Ev));
                  end if;
                  return Fail_Process;
               end if;
            -- else not interesting.
            end if;
         end loop;
         if Start = 0 then
            -- No execution found.
            if Grading_Data.Manual_Grading_Requested_for_Test (
               Grading_Data.Summary_of_Tests(First_Item).Source_Name) then
               -- We allow this so that tests that require special linking
               -- can be excluded from the results.
               Message_For_Event (Kind => Special_Action,
                                  Item => First_Item,
                                  Event=> 0, -- No event.
                                  Text =>  "is mising execution, manual " &
                                           "grading requested");
               return Manual_Grading;
            else
               Message_For_Event (Kind => Failed,
                                  Item => First_Item,
                                  Event=> 0, -- No event.
                                  Text => "is missing execution");
               return Fail_Run_Missing;
            end if;
         -- else found exactly one execution start.
         end if;

         -- Check that there is exactly one Execution_End:
         Start := 0;
         for Ev in First_Event .. Last_Event loop
            if Grading_Data.Event_Trace(Ev).Event = Trace.Execution_End then
               if Start = 0 then
                  Start := Ev;
               else
                  Message_For_Event (Kind => Failed,
                                     Item => First_Item,
                                     Event=> 0, -- No event.
                                     Text =>
                         "has multiple execution completion events");
                  if Noise_Level /= Quiet then
                     Ada.Text_IO.Put_Line ("  First run=" &
                        Grading_Data.Event_Count'Image (Start) &
                        "; Second run=" &
                        Grading_Data.Event_Count'Image (Ev));
                  end if;
                  return Fail_Process;
               end if;
            -- else not interesting.
            end if;
         end loop;
         if Start = 0 then
            -- No execution completion found, assume a crash.
            Message_For_Event (Kind => Failed,
                               Item => First_Item,
                               Event=> 0, -- No event.
                               Text => "crashed during execution");
            return Fail_Run_Crash;
         -- else found exactly one execution completion.
         end if;

         -- Now, figure a result based on any execution messages:
         -- Failed messages have priority, then Not-Applicable messages,
         -- and Special Handling are least important. If there are no messages
         -- of any kind, the test passes (pending the completion check).
         -- (Note that we don't keep Comment messages).
         for Ev in First_Event .. Last_Event loop
            if Grading_Data.Event_Trace(Ev).Event =
               Trace.Execution_Failure then
               if Grading_Data.Manual_Grading_Requested_for_Test (
                    Grading_Data.Summary_of_Tests(First_Item).Source_Name) then
                  Message_For_Event (Kind => Special_Action,
                                     Item => First_Item,
                                     Event=> Ev,
                                     Text =>  "reports failure, manual " &
                                              "grading requested");
                  return Manual_Grading;
               else
                  Message_For_Event (Kind => Failed,
                                     Item => First_Item,
                                     Event=> Ev,
                                     Text =>  "reports failure");
                  return Fail_Run_Message;
               end if;
            elsif Grading_Data.Event_Trace(Ev).Event =
               Trace.Execution_Not_Applicable then
               Ex_Result := NA_Run_Message;
               -- We don't report this immediately, as a later failed message
               -- overrides this.
               if Noise_Level /= Quiet then
                  Message_For_Event (Kind => NA,
                                     Item => First_Item,
                                     Event=> Ev,
                                     Text =>  "reports not-applicable");
               end if;
            elsif Grading_Data.Event_Trace(Ev).Event =
               Trace.Execution_Special_Action then
               if Ex_Result = Pass_Run then
                  Ex_Result := SH_Run_Message;
                  -- We don't return this result immediately, as a later failed
                  -- or not applicable message overrides this. But we
                  -- always report special handling so that it doesn't get
                  -- lost.
                  Message_For_Event (Kind => Special_Action,
                                     Item => First_Item,
                                     Event=> Ev,
                                     Text =>
                     "reports that it needs special handling");
               -- else already some other result.
               end if;
            -- else not interesting.
            end if;
         end loop;

         if Ex_Result = Pass_Run and then Noise_Level /= Quiet then
             Message_For_Event (Kind => Passed,
                                Item => First_Item,
                                Event=> 0, -- No event.
                                Text => "passed execution");
         -- else other results were already reported if they will be.
         end if;
         return Ex_Result;
      end;
   end Grade_Test;


begin
   Ada.Text_IO.Put_Line ("ACATS Grading Tool - version 1.0");

   if Ada.Command_Line.Argument_Count < 4 then
      Ada.Text_IO.Put_Line ("*ERROR* Insufficient command line arguments - " &
         "event trace file, ");
      Ada.Text_IO.Put_Line ("        test summary file, manual grading file," &
         " and grading name");
      Ada.Text_IO.Put_Line ("        string are required - quitting");
      raise Kill_Tool;
   end if;

   --if Noise_Level /= Verbose then
   --    for I in 1 .. Ada.Command_Line.Argument_Count loop
   --       Ada.Text_IO.Put_Line ("Arg(" & Natural'Image(I) & ")=" &
   --          Ada.Command_Line.Argument(I));
   --    end loop;
   --end if;

   -- Process the options.
   for I in 5 .. Ada.Command_Line.Argument_Count loop
      Process_Option (Ada.Command_Line.Argument(I));
   end loop;
   if Noise_Level = Unset then -- Option didn't appear.
      Noise_Level := Normal;
   end if;
   if Compile_Check_Level = Unset then -- Option didn't appear.
      Compile_Check_Level := Check_All;
   end if;

   -- Read the files.
   begin
      Grading_Data.Read_Event_Trace (Ada.Command_Line.Argument(1));
   exception
      when Exc1:Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("*ERROR* Event Trace file not found - " &
            "message: " & Ada.Exceptions.Exception_Message (Exc1));
         raise Kill_Tool;
      when Exc2:Grading_Data.Format_Error =>
         Ada.Text_IO.Put_Line ("*ERROR* Event Trace file malformed - " &
            Ada.Exceptions.Exception_Message (Exc2));
         raise Kill_Tool;
      when Exc3:others =>
         if Noise_Level = Verbose then
            Ada.Text_IO.Put_Line ("*ERROR* Exception processing Event Trace " &
               "file - " & Ada.Exceptions.Exception_Information (Exc3));
         else
            Ada.Text_IO.Put_Line ("*ERROR* Exception processing Event " &
               "Trace file - " & Ada.Exceptions.Exception_Name (Exc3) & "; " &
               Ada.Exceptions.Exception_Message (Exc3));
         end if;
         raise Kill_Tool;
   end;

   begin
      Grading_Data.Read_Summary_of_Tests (Ada.Command_Line.Argument(2));
   exception
      when Exc1:Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("*ERROR* Test Summary file not found - " &
            "message: " & Ada.Exceptions.Exception_Message (Exc1));
         raise Kill_Tool;
      when Exc2:Grading_Data.Format_Error =>
         Ada.Text_IO.Put_Line ("*ERROR* Test Summary file malformed - " &
            Ada.Exceptions.Exception_Message (Exc2));
         raise Kill_Tool;
      when Exc3:others =>
         if Noise_Level = Verbose then
            Ada.Text_IO.Put_Line ("*ERROR* Exception processing Test " &
               "Summary file - " &
               Ada.Exceptions.Exception_Information (Exc3));
         else
            Ada.Text_IO.Put_Line ("*ERROR* Exception processing Test " &
               "Summary file - " & Ada.Exceptions.Exception_Name (Exc3) & "; "
               & Ada.Exceptions.Exception_Message (Exc3));
         end if;
         raise Kill_Tool;
   end;

   begin
      Grading_Data.Read_Manual_Grading_List (Ada.Command_Line.Argument(3));
   exception
      when Exc1:Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("*ERROR* Manual Grading file not found - " &
            "message: " & Ada.Exceptions.Exception_Message (Exc1));
         raise Kill_Tool;
      when Exc2:Grading_Data.Format_Error =>
         Ada.Text_IO.Put_Line ("*ERROR* Manual Grading file malformed - " &
            Ada.Exceptions.Exception_Message (Exc2));
         raise Kill_Tool;
      when Exc3:others =>
         if Noise_Level = Verbose then
            Ada.Text_IO.Put_Line ("*ERROR* Exception processing Manual " &
               "Grading file - " &
               Ada.Exceptions.Exception_Information (Exc3));
         else
            Ada.Text_IO.Put_Line ("*ERROR* Exception processing Manual " &
               "Grading file - " & Ada.Exceptions.Exception_Name (Exc3) & "; "
               & Ada.Exceptions.Exception_Message (Exc3));
         end if;
         raise Kill_Tool;
   end;

   -- Display start state:
   if Noise_Level /= Quiet then
       Ada.Text_IO.Put_Line ("  Compile Checks: " &
          Compile_Check_Type'Image(Compile_Check_Level));
       if Use_Time_Stamps then
          Ada.Text_IO.Put_Line ("  Make checks of time stamps in event log");
       else
          Ada.Text_IO.Put_Line ("  Omit checks of time stamps in event log");
       end if;
       if Use_Positions then
          Ada.Text_IO.Put_Line ("  Use positions of error messages when " &
             "grading");
       else
          Ada.Text_IO.Put_Line ("  Use only line numbers of error messages " &
             "when grading");
       end if;
       Ada.Text_IO.Put_Line ("  Report verbosity: " &
          Noise_Type'Image(Noise_Level));
       Ada.Text_IO.Put_Line (" " & Grading_Data.Event_Count'Image
          (Grading_Data.Last_Event) & " event trace records read from " &
          Ada.Command_Line.Argument(1));
       Ada.Text_IO.Put_Line (" " & Grading_Data.Summary_Item_Count'Image
          (Grading_Data.Last_Summary_Item) & " test summary trace records " &
          "read from " & Ada.Command_Line.Argument(2));
       Ada.Text_IO.Put_Line (" " & Grading_Data.Manual_Grading_Count'Image
          (Grading_Data.Last_Manual_Grading) & " manual grading requests " &
          "read from " & Ada.Command_Line.Argument(3));
   end if;

   -- Make sure that all of the test summaries are grouped by test and in
   -- line number order within a source file.
   -- This is just a precaution; they should generally be in the right order.

   declare
      function "<" (Left, Right : Test_Summary.Info_Record) return Boolean is
         use type Trace.Line_Number_Type, Trace.Line_Position_Type;
         use type Test_Summary.Info_Kind_Type;
      begin
         if Left.Source_Name < Right.Source_Name then
            return True;
         elsif Left.Source_Name > Right.Source_Name then
            return False;
         -- If we're here, the two records are for the same source file.
         elsif Left.Start_Line < Right.Start_Line then
            return True;
         elsif Left.Start_Line > Right.Start_Line then
            return False;
         -- If we're here, the two records have the same line.
         -- Put the compilation unit first, in case an error tag is in the
         -- context clause (they could have the same position).
         elsif Left.Kind = Test_Summary.Compilation_Unit and then
               Right.Kind /= Test_Summary.Compilation_Unit then
            return True;
         elsif Left.Kind /= Test_Summary.Compilation_Unit and then
               Right.Kind = Test_Summary.Compilation_Unit then
            return False;
         elsif Left.Start_Position >= Right.Start_Position then
            return False;
         else -- Left.Start_Position < Right.Start_Position then
            return True;
         end if;
      end "<";

      procedure Sort_Summary is new Ada.Containers.Generic_Array_Sort (
         Index_Type   => Grading_Data.Summary_Item_Index,
         Element_Type => Test_Summary.Info_Record,
         Array_Type   => Grading_Data.Summary_Item_Array,
         "<"          => "<");
   begin
      Sort_Summary (Grading_Data.Summary_of_Tests(
         Grading_Data.Summary_of_Tests'First ..
                                            Grading_Data.Last_Summary_Item));
   end;

   -- Make sure that all of the events are grouped by test and in
   -- time stamp order within a test, and finally, by the kinds and
   -- line numbers where we have them. Unlike the summaries, we can't expect
   -- these to be in the right order (as they come from the
   -- test taker, and they may not run things in the canonical order), nor
   -- can we assume that the timestamps will differentiate events (many
   -- events can happen in 10 milliseconds, even if they are accurate,
   -- which we also can't assume).

   -- The "test" is the first 7 characters of the Name component;
   -- the source names, main names, and reported execution name should
   -- all include this.

   declare
      function "<" (Left, Right : Trace.Event_Record) return Boolean is
         use type Ada.Calendar.Time;
         use type Trace.Event_Type;
      begin
         if Left.Name(1..7) < Right.Name(1..7) then
            return True;
         elsif Left.Name(1..7) > Right.Name(1..7) then
            return False;
         elsif Left.Timestamp < Right.Timestamp then
            return True;
         elsif Left.Timestamp > Right.Timestamp then
            return False;
         -- If we get here, the timestamps are the same.
         elsif Left.Event not in Trace.Compile_Event and then
               Right.Event not in Trace.Compile_Event then
            -- There should only be a single set of binder events,
            -- and similarly for execution events. So ordering them
            -- by the event kind is good enough.
            return Left.Event < Right.Event;
         elsif Left.Event in Trace.Compile_Event and then
               Right.Event not in Trace.Compile_Event then
            -- Put compile events first.
            return True;
         elsif Left.Event not in Trace.Compile_Event and then
               Right.Event in Trace.Compile_Event then
            -- Put compile events first.
            return False;
         -- If we get here, both sides have compilation events.
         elsif Left.Event = Trace.Compilation_Start and then
             Right.Event = Trace.Compilation_Start then
            return Left.Start_Line < Right.Start_Line;
         elsif Left.Event = Trace.Compilation_Start and then
             Right.Event in Trace.Compile_Error ..
                                                Trace.Compile_Warning then
            return Left.Start_Line < Right.Error_Line;
         elsif Left.Event in Trace.Compile_Error .. Trace.Compile_Warning
             and then Right.Event in Trace.Compile_Error ..
                                                Trace.Compile_Warning then
            if Left.Error_Line = Right.Error_Line then
               return Left.Error_Position < Right.Error_Position;
            else
               return Left.Error_Line < Right.Error_Line;
            end if;
         elsif Left.Event in Trace.Compile_Error .. Trace.Compile_Warning
             and then Right.Event = Trace.Compilation_Start then
            return Left.Error_Line < Right.Start_Line;
         elsif Left.Event = Trace.Compilation_End and then
            Right.Event /= Trace.Compilation_End then
            -- Put the end after the others.
            return False;
         elsif Left.Event /= Trace.Compilation_End and then
            Right.Event = Trace.Compilation_End then
            -- Put the end after the others.
            return True;
         else
            -- If we get here, both are Compilation_Ends, and there is nothing
            -- left to compare. So they're equal.
            return Left.Event < Right.Event;
         end if;
      end "<";

      procedure Sort_Events is new Ada.Containers.Generic_Array_Sort (
         Index_Type   => Grading_Data.Event_Index,
         Element_Type => Trace.Event_Record,
         Array_Type   => Grading_Data.Event_Trace_Array,
         "<"          => "<");
   begin
      Sort_Events (Grading_Data.Event_Trace(
         Grading_Data.Event_Trace'First .. Grading_Data.Last_Event));
   end;

   if DEBUG and then Noise_Level = Verbose then
      Ada.Text_IO.Put_Line ("Event trace as loaded and sorted");
      for I in Grading_Data.Event_Index'First .. Grading_Data.Last_Event loop
        Grading_Data.Put_Event_Line (I);
      end loop;
      Ada.Text_IO.Put_Line ("Test summaries as loaded and sorted");
      for I in Grading_Data.Summary_Item_Index'First ..
        Grading_Data.Last_Summary_Item loop
         Grading_Data.Put_Summary_Item_Line (I);
      end loop;
   end if;

   -- Main grading loop:

   -- We only grade tests that are found in the summary items. We'll
   -- ignore events for other tests, so that it isn't necessary to have
   -- exactly matching runs in order to successfully use the tool.
   --
   -- On the other hand, we will grade all of the tests found in the
   -- summary items, whether or not they appear in the event list.
   -- (If not, they should come up as missing.)
   --
   -- We need to work on sets of events and summary items for a single
   -- test at one time. So we will loop through the entire set of
   -- of summary items, finding groups that are for a single test (since
   -- we previously sorted these, they are together).
   --
   -- Once we've found the events (if any) and summary items, we'll
   -- pass them to the grading routine. See that for more details.
   --
   -- Special note: We do *not* try to check foundations (those do not have the
   -- test name in their prefix, since they're shared with several tests).
   -- Thus, we allow those to be compiled separately (into a support library,
   -- for instance) or interspersed with tests (compiled individually with
   -- related tests). Generally, a failed foundation will lead naturally to a
   -- compile error (and failed test) anyway. For formal testing, the ACAL
   -- should hand check that the foundations compiled - if separate, check a
   -- listing of the foundation compiles, and otherwise do some spot checks.

   declare
      use type Grading_Data.Summary_Item_Count; -- For operators.
      use type Grading_Data.Event_Count;        -- For operators.
      Working_Item : Grading_Data.Summary_Item_Count :=
         Grading_Data.Summary_of_Tests'First;
      First_Item : Grading_Data.Summary_Item_Count;
      First_Event :  Grading_Data.Event_Count := 1;
      Last_Event :  Grading_Data.Event_Count := 0;
      Result : Result_Type := Unknown;
   begin
      while Working_Item <= Grading_Data.Last_Summary_Item loop
         First_Item := Working_Item;
         Working_Item := Working_Item + 1;
         while Working_Item <= Grading_Data.Last_Summary_Item loop
            exit when Grading_Data.Summary_of_Tests(First_Item).
               Source_Name(1..7) /=
               Grading_Data.Summary_of_Tests(Working_Item).Source_Name(1..7);
            Working_Item := Working_Item + 1;
         end loop;
         -- When we get here, all of the items First_Item .. Working_Item - 1
         -- belong to the same test.

         -- Set the events to a null range in case none are found.
         First_Event := 1;
         Last_Event := 0;
         Outer: for Ev in Grading_Data.Event_Trace'First ..
                   Grading_Data.Last_Event loop
            if Grading_Data.Event_Trace (Ev).Name(1..7) =
               Grading_Data.Summary_of_Tests(First_Item).Source_Name(1..7) then
               First_Event := Ev;
               Last_Event := Ev;
             Inner: for Ev_Inner in Ev+1 .. Grading_Data.Last_Event loop
                  if Grading_Data.Event_Trace (Ev_Inner).Name(1..7) =
                     Grading_Data.Summary_of_Tests(First_Item).
                        Source_Name(1..7) then
                     Last_Event := Ev_Inner;
                  else -- We're done (we assume the events are consecutive,
                       -- we sorted them to ensure that).
                     exit Outer;
                  end if;
               end loop Inner;
               -- If we get here, this test includes the last event of those
               -- stored. We're also done here.
               exit Outer;
            -- else no match.
            end if;
         end loop Outer;

         Result := Grade_Test (First_Item, Working_Item - 1,
                               First_Event, Last_Event);

         -- We assume that Grade_Test reported any problems, as required
         -- by the Noise_Level.

         if Result = Unknown then
            -- This shouldn't happen; we'll treat it as a process failure.
            Result := Fail_Process;
            if Noise_Level /= Quiet then
               Ada.Text_IO.Put_Line ("%% Unknown result for test " &
                  Grading_Data.Summary_of_Tests(First_Item).Source_Name(1..7));
            end if;
         -- else we have a sensible result.
         end if;

         Overall_Summary(Total)  := Overall_Summary(Total) + 1;
         Overall_Summary(Result) := Overall_Summary(Result) + 1;
         if Grading_Data.Summary_of_Tests(First_Item).Source_Name(1) = 'B' then
            B_Test_Summary(Total)  := B_Test_Summary(Total) + 1;
            B_Test_Summary(Result) := B_Test_Summary(Result) + 1;
         elsif Grading_Data.Summary_of_Tests(First_Item).Source_Name(1) =
            'C' then
            C_Test_Summary(Total)  := C_Test_Summary(Total) + 1;
            C_Test_Summary(Result) := C_Test_Summary(Result) + 1;
         elsif Grading_Data.Summary_of_Tests(First_Item).Source_Name(1) =
            'L' then
            L_Test_Summary(Total)  := L_Test_Summary(Total) + 1;
            L_Test_Summary(Result) := L_Test_Summary(Result) + 1;
         else
            Other_Test_Summary(Total)  := Other_Test_Summary(Total) + 1;
            Other_Test_Summary(Result) := Other_Test_Summary(Result) + 1;
         end if;

      end loop;
   end;


   -- Display the summary report. This is unconditional (even in Quiet mode).

   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.Put_Line ("=======================================");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Summary for " & Ada.Command_Line.Argument(4));
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line     ("Result                       Overall  " &
                             "B-Tests  C-Tests  L-Tests  Other Tests");
   Ada.Text_IO.New_Line;
   for Res in Result_Type loop
      case Res is
         when Total =>
            Ada.Text_IO.Put ("   Total Tests Graded        ");
         when Fail_Process =>
            Ada.Text_IO.Put ("** Failed (Process)          ");
         when Fail_Compile_Missing =>
            Ada.Text_IO.Put ("** Failed (Compile Missing)  ");
         when Fail_Compile_Crash =>
            Ada.Text_IO.Put ("** Failed (Compiler Crash)   ");
         when Fail_Compile_Error =>
            Ada.Text_IO.Put ("** Failed (Unexpected Error) ");
         when Fail_Compile_OK_Error =>
            Ada.Text_IO.Put ("** Failed (Error in OK area) ");
         when Fail_Compile_Missing_Error =>
            Ada.Text_IO.Put ("** Failed (Missing Error)    ");
         when Pass_Compile_Error =>
            Ada.Text_IO.Put ("== Passed (Expected Errors)  ");
         when NA_Compile_Error =>
            Ada.Text_IO.Put ("++ Not-Applicable (Compile)  ");
         when NA_Annex_C_Error =>
            Ada.Text_IO.Put ("++ Not-Applicable (Annex C)  ");
         when Fail_Bind_Missing =>
            Ada.Text_IO.Put ("** Failed (Bind Missing)     ");
         when Fail_Bind_Crash =>
            Ada.Text_IO.Put ("** Failed (Bind Crash)       ");
         when Fail_Bind_Error =>
            Ada.Text_IO.Put ("** Failed (Bind Error)       ");
         when Pass_Bind_Error =>
            Ada.Text_IO.Put ("== Passed (Bind Error)       ");
         when Fail_Run_Missing =>
            Ada.Text_IO.Put ("** Failed (Run Missing)      ");
         when Fail_Run_Crash =>
            Ada.Text_IO.Put ("** Failed (Run Crash)        ");
         when Fail_Run_Message =>
            Ada.Text_IO.Put ("** Failed (Runtime Message)  ");
         when NA_Run_Message =>
            Ada.Text_IO.Put ("++ Not-Applicable (Runtime)  ");
         when SH_Run_Message =>
            Ada.Text_IO.Put ("!! Special Handling (Runtime)");
         when Manual_Grading =>
            Ada.Text_IO.Put ("!! Manual Grading Requested  ");
         when Pass_Run =>
            Ada.Text_IO.Put ("== Passed (Runtime)          ");
      end case;
      Int_IO.Put (Overall_Summary(Res), 6);
      Int_IO.Put (B_Test_Summary(Res),  9);
      Int_IO.Put (C_Test_Summary(Res),  9);
      Int_IO.Put (L_Test_Summary(Res),  9);
      Int_IO.Put (Other_Test_Summary(Res), 9);
      Ada.Text_IO.New_Line;
      if Res = Total then
         Ada.Text_IO.New_Line;
      end if;
   end loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("** Total Failed              ");
   Int_IO.Put (Overall_Summary(Fail_Process) +
               Overall_Summary(Fail_Compile_Missing) +
               Overall_Summary(Fail_Compile_Crash) +
               Overall_Summary(Fail_Compile_OK_Error) +
               Overall_Summary(Fail_Compile_Error) +
               Overall_Summary(Fail_Compile_Missing_Error) +
               Overall_Summary(Fail_Bind_Missing) +
               Overall_Summary(Fail_Bind_Crash) +
               Overall_Summary(Fail_Bind_Error) +
               Overall_Summary(Fail_Run_Missing) +
               Overall_Summary(Fail_Run_Crash) +
               Overall_Summary(Fail_Run_Message), 6);
   Int_IO.Put (B_Test_Summary(Fail_Process) +
               B_Test_Summary(Fail_Compile_Missing) +
               B_Test_Summary(Fail_Compile_Crash) +
               B_Test_Summary(Fail_Compile_OK_Error) +
               B_Test_Summary(Fail_Compile_Error) +
               B_Test_Summary(Fail_Compile_Missing_Error) +
               B_Test_Summary(Fail_Bind_Missing) +
               B_Test_Summary(Fail_Bind_Crash) +
               B_Test_Summary(Fail_Bind_Error) +
               B_Test_Summary(Fail_Run_Missing) +
               B_Test_Summary(Fail_Run_Crash) +
               B_Test_Summary(Fail_Run_Message), 9);
   Int_IO.Put (C_Test_Summary(Fail_Process) +
               C_Test_Summary(Fail_Compile_Missing) +
               C_Test_Summary(Fail_Compile_Crash) +
               C_Test_Summary(Fail_Compile_OK_Error) +
               C_Test_Summary(Fail_Compile_Error) +
               C_Test_Summary(Fail_Compile_Missing_Error) +
               C_Test_Summary(Fail_Bind_Missing) +
               C_Test_Summary(Fail_Bind_Crash) +
               C_Test_Summary(Fail_Bind_Error) +
               C_Test_Summary(Fail_Run_Missing) +
               C_Test_Summary(Fail_Run_Crash) +
               C_Test_Summary(Fail_Run_Message), 9);
   Int_IO.Put (L_Test_Summary(Fail_Process) +
               L_Test_Summary(Fail_Compile_Missing) +
               L_Test_Summary(Fail_Compile_Crash) +
               L_Test_Summary(Fail_Compile_OK_Error) +
               L_Test_Summary(Fail_Compile_Error) +
               L_Test_Summary(Fail_Compile_Missing_Error) +
               L_Test_Summary(Fail_Bind_Missing) +
               L_Test_Summary(Fail_Bind_Crash) +
               L_Test_Summary(Fail_Bind_Error) +
               L_Test_Summary(Fail_Run_Missing) +
               L_Test_Summary(Fail_Run_Crash) +
               L_Test_Summary(Fail_Run_Message), 9);
   Int_IO.Put (Other_Test_Summary(Fail_Process) +
               Other_Test_Summary(Fail_Compile_Missing) +
               Other_Test_Summary(Fail_Compile_Crash) +
               Other_Test_Summary(Fail_Compile_OK_Error) +
               Other_Test_Summary(Fail_Compile_Error) +
               Other_Test_Summary(Fail_Compile_Missing_Error) +
               Other_Test_Summary(Fail_Bind_Missing) +
               Other_Test_Summary(Fail_Bind_Crash) +
               Other_Test_Summary(Fail_Bind_Error) +
               Other_Test_Summary(Fail_Run_Missing) +
               Other_Test_Summary(Fail_Run_Crash) +
               Other_Test_Summary(Fail_Run_Message), 9);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("++ Total Not-Applicable      ");
   Int_IO.Put (Overall_Summary(NA_Run_Message) +
               Overall_Summary(NA_Compile_Error) +
               Overall_Summary(NA_Annex_C_Error), 6);
   Int_IO.Put (B_Test_Summary(NA_Run_Message) +
               B_Test_Summary(NA_Compile_Error) +
               B_Test_Summary(NA_Annex_C_Error), 9);
   Int_IO.Put (C_Test_Summary(NA_Run_Message) +
               C_Test_Summary(NA_Compile_Error) +
               C_Test_Summary(NA_Annex_C_Error), 9);
   Int_IO.Put (L_Test_Summary(NA_Run_Message) +
               L_Test_Summary(NA_Compile_Error) +
               L_Test_Summary(NA_Annex_C_Error), 9);
   Int_IO.Put (Other_Test_Summary(NA_Run_Message) +
               Other_Test_Summary(NA_Compile_Error) +
               Other_Test_Summary(NA_Annex_C_Error), 9);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("!! Total Special             ");
   Int_IO.Put (Overall_Summary(SH_Run_Message) +
               Overall_Summary(Manual_Grading), 6);
   Int_IO.Put (B_Test_Summary(SH_Run_Message) +
               B_Test_Summary(Manual_Grading), 9);
   Int_IO.Put (C_Test_Summary(SH_Run_Message) +
               C_Test_Summary(Manual_Grading), 9);
   Int_IO.Put (L_Test_Summary(SH_Run_Message) +
               L_Test_Summary(Manual_Grading), 9);
   Int_IO.Put (Other_Test_Summary(SH_Run_Message) +
               Other_Test_Summary(Manual_Grading), 9);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("== Total Passed              ");
   Int_IO.Put (Overall_Summary(Pass_Run) + Overall_Summary(Pass_Compile_Error)
               + Overall_Summary(Pass_Bind_Error), 6);
   Int_IO.Put (B_Test_Summary(Pass_Run) + B_Test_Summary(Pass_Compile_Error)
               + B_Test_Summary(Pass_Bind_Error), 9);
   Int_IO.Put (C_Test_Summary(Pass_Run) + C_Test_Summary(Pass_Compile_Error)
               + C_Test_Summary(Pass_Bind_Error), 9);
   Int_IO.Put (L_Test_Summary(Pass_Run) + L_Test_Summary(Pass_Compile_Error)
               + L_Test_Summary(Pass_Bind_Error), 9);
   Int_IO.Put (Other_Test_Summary(Pass_Run) +
               Other_Test_Summary(Pass_Compile_Error) +
               Other_Test_Summary(Pass_Bind_Error), 9);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("=======================================");
   Ada.Text_IO.New_Line;
   if Overall_Summary(Total) =
      Overall_Summary(Pass_Compile_Error) +
      Overall_Summary(NA_Compile_Error) +
      Overall_Summary(Pass_Bind_Error) +
      Overall_Summary(NA_Run_Message) +
      Overall_Summary(SH_Run_Message) +
      Overall_Summary(Manual_Grading) +
      Overall_Summary(Pass_Run) then
      -- The total number of tests is the same as those in one of the
      -- passing statuses.
      Ada.Text_IO.Put_Line ("Overall result for " &
         Ada.Command_Line.Argument(4) & " is PASSED");
      if Overall_Summary(SH_Run_Message) /= 0 then
         if Overall_Summary(Manual_Grading) /= 0 then
            Ada.Text_IO.Put_Line ("   pending special handling results and");
            Ada.Text_IO.Put_Line ("   manual grading of" &
               Natural'Image(Overall_Summary(Manual_Grading)) & " tests.");
         else
            Ada.Text_IO.Put_Line ("   pending special handling results.");
         end if;
      elsif Overall_Summary(Manual_Grading) /= 0 then
         Ada.Text_IO.Put_Line ("   pending manual grading of" &
            Natural'Image(Overall_Summary(Manual_Grading)) & " tests.");
      end if;
   else
      Ada.Text_IO.Put_Line ("Overall result for " &
         Ada.Command_Line.Argument(4) & " is **FAILED**");
      if Overall_Summary(Total) -
         (Overall_Summary(NA_Run_Message) +
          Overall_Summary(NA_Compile_Error) +
          Overall_Summary(NA_Annex_C_Error) +
          Overall_Summary(SH_Run_Message)) > 100 then
         -- Calculate a "progress" score. This score is on a scale of 0 to
         -- 100, and judges how close to perfect a non-passing implementation
         -- is to passing. The score ignores Not Applicable and runtime
         -- Special_Handling tests and gives 1/2 credit to manual graded tests
         -- (one should prefer to reduce the number of those if possible).
         -- We only give this score when many tests are run (it's mainly
         -- intended to give a progress score to the ACATS as a whole).
         declare
            package Flt_IO is new Ada.Text_IO.Float_IO (Float);
            Total_Float : Float;
            Passed_Float : Float;
         begin
            Total_Float := Float(Overall_Summary(Total) -
                                 (Overall_Summary(NA_Run_Message) +
                                 Overall_Summary(NA_Compile_Error) +
                                 Overall_Summary(NA_Annex_C_Error) +
                                 Overall_Summary(SH_Run_Message)));
            Passed_Float := Float (Overall_Summary(Pass_Run) +
                                   Overall_Summary(Pass_Compile_Error) +
                                   Overall_Summary(Pass_Bind_Error)) +
                            Float (Overall_Summary(Manual_Grading))*0.5;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put ("  Progress Score (on a 0 to 100 scale) for " &
                             "this implementation is");
            Flt_IO.Put ((Passed_Float/Total_Float)*100.0, 5, 2, 0);
            Ada.Text_IO.New_Line;
         end;
      end if;
   end if;

exception
   when Kill_Tool => null; -- A fatal error happened.
end Grade;
