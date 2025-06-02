-- TRACE.A
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
--      This package defines the data structures that represent a trace of
--      the compilation, binding, and execution of a set of ACATS tests.
--      This trace records can then be used to grade the results of an
--      ACATS test run.
--
-- CHANGE HISTORY:
--      29 Feb 2016   RLB  Created package. (Happy Leap Day!)

with Ada.Strings.Unbounded;
with Ada.Calendar;
package Trace is

   -- The basic idea is that each (interesting) event that occurs during
   -- the compilation, binding/linking, and execution of an ACATS test is
   -- recorded as a series of event records, one per event. We define a
   -- .CSV file format for saving these events; this file will be created
   -- by some method outside of the ACATS as it will depend upon the
   -- implementation. (Peferably the .CSV event trace file will be created
   -- using tools or options provided by the implementor, but they also could
   -- be constructed by text processing of compiler/binder output). The
   -- ACAA has provided an enhanced version of the Report package in order
   -- to provide automatic creation of an event trace file from the execution
   -- of an ACATS test.
   --
   -- For the purposes of these event traces, "binding" is the operation of
   -- readying a partition for execution; it is the phase that enforces
   -- post-compilation rules. This phase may or may not include a traditional
   -- "linking" phase. If linking is separate from binding, we do not track
   -- events relative to linking. That is mainly because such a separate
   -- linking phase should not be able to fail (since the post-compilation
   -- checks, if any, have already been made in the binding phase), and
   -- secondarily because such a linking phase typically involves some
   -- third-party tool which would be relatively difficult to modify to
   -- generate event trace records. Since any failure in the linking phase
   -- would necessarily prevent execution of a test, such a failure would still
   -- show up in the event trace. Thus, there doesn't seem to be any need to
   -- include the linking phase (if any) in the event trace.

   -- We include an Unknown event for ease of programming: almost all
   -- programming constructs need a "not known yet" value (for instance,
   -- null for access types), and we expect that these event trace records
   -- will be no different.

   -- We don't include an event to record calls to Report.Comment, as those
   -- have no effect on grading.

   type Event_Type is (
      Unknown,
      Compilation_Start,
      Compile_Error,
      Compile_Warning,  -- Optional: Not used by grading tools.
      Compilation_End,
      Binder_Start,
      Binder_Error,
      Binder_Warning,   -- Optional: Not used by grading tools.
      Binder_End,
      Execution_Start,
      Execution_Failure,
      Execution_Not_Applicable,
      Execution_Special_Action,
      Execution_End);

   subtype Compile_Event is Event_Type range
                                       Compilation_Start .. Compilation_End;
   subtype Binder_Event is Event_Type range
                                       Binder_Start .. Binder_End;
   subtype Execution_Event is Event_Type range
                                   Execution_Start .. Execution_End;

   subtype Name_Subtype is String(1..12);
      -- ACATS test source files are all named with 8.3 names.
      -- ACATS tests arre named with 8 (or less) character names,
      -- with the exception of a trailing 'M' in some legacy tests.
      -- Thus 12 characters is always enough to store these values.

   subtype Line_Number_Type is Natural range 0 .. 4000;
      -- No ACATS test file has more than 4000 lines, so we limit
      -- line numbers appropriately.

   subtype Line_Position_Type is Natural range 0 .. 255;
      -- No ACATS test file has lines longer than 255 characters, so
      -- we limit line positions appropriately.
      -- Note: We could get this value from Macro.Dfs [MAX_IN_LEN], but that
      -- would add at lot of compilation for little value. If an implementation
      -- has MAX_IN_LEN greater than 255, it can modify this value
      -- appropriately.

   type Event_Record (Event : Event_Type := Unknown) is record
       Timestamp : Ada.Calendar.Time;
       Name : Name_Subtype;
          -- For Compilation_xxx events, this is the simple name of the
          -- source file.
          -- For Binder_xxx events, this is the name of the main subprogram.
          -- For Execution_xxx events, this is the name of the test as
          -- passed to Report.Test.
       Message : Ada.Strings.Unbounded.Unbounded_String;
          -- Note: Messages are not used for test grading; they are included
          -- in the trace to improve the reports from the various tools
          -- and as a potential debugging aid for the implementer.
          --
          -- For Compile_Error, Compile_Warning, Binder_Error, and
          -- Binder_Warning events, this is the message emitted by the compiler
          -- or binder. The message will need to have any double quotes
          -- eliminated (a limitation of a CSV format) and should be simplified
          -- if it is very long.
          --
          -- For Execution_Start, Execution_Failure, Execution_Not_Applicable,
          -- and Execution_Special_Action events, this is the string passed
          -- into the appropriate Report routine. Report.A truncates the string
          -- at 160 characters to limit the file sizes.
          --
          -- For Compilation_End, Binder_End, and Execution_End, this is either
          -- null or an implementation-defined result for the operation
          -- (OK, with Errors, Passed, Failed, etc.)
          --
          -- For other events, this component is not used and should be set
          -- to null.
          --
          -- We use an Unbounded_String here to ease memory management and
          -- to avoid having to set a hard length limit on the messages. (Some
          -- of the ones passed to Report are quite long.)

       case Event is
          when Compilation_Start =>
             Start_Line : Line_Number_Type;
             -- The first line of the current compilation unit. Usually 1,
             -- unless there are multiple compilation units in a single file.

          when Compile_Error | Compile_Warning =>
             Error_Line     : Line_Number_Type;
             Error_Position : Line_Position_Type;
                -- Note: The Error_Position is not used in formal test
                -- grading; but we can use it for strict checking to ensure
                -- that both the grading tools and implementation are high
                -- quality.

          when others => null;
        end case;
   end record;

   -- Notes: I could have used a tagged type structure instead of a record
   -- type with variants, but that maps no better to .CSV file (which has a
   -- field for every value in every record), and as we aren't planning to
   -- define any primitive operations on these records, there doesn't seem
   -- to be any advantage in doing so -- but there are disadvantages
   -- (Root_Event_Record'Class is indefinite, while Event_Record is definite;
   -- there is extra space used for a tag even though we won't get much, if
   -- any, use out of that tag).

   -- Since a .csv file has a field (a "column") for every component, we've
   -- combined the components to keep the number of columns in check.
   -- Perhaps it would have been better to put the Message component into
   -- variants (although that would require splitting some of the existing
   -- variants, thus forcing some of the component names to change).


   -- .CSV Event Trace files:
   --
   -- The .CSV (Comma Separated Value) file is commonly used by spreadsheets
   -- and database programs. It consists of a set of values, separated by
   -- commas, with one line per record. The values can be anything so long as
   -- it does not contain a comma (it's recommended that values don't contain
   -- spaces, tabs, or semicolons, either, so that the files can be read by
   -- as many spreadsheets and databases as possible). The values can be
   -- quoted with double quotes, but in that case the value cannot contain
   -- a double quote (commas, semicolons, tabs, and spaces are OK then).
   --
   -- We chose .CSV because it
   --   (1) is a pure text format that is easy to write in Ada; open the file
   --       with Text_IO in Append_Mode, Put_Line the line of values for the
   --       record, then close the file. No end markers need to be moved.
   --   (2) can be read by many spreadsheet programs; thus it is easy to
   --       check for errors in the file (load it into a spreadsheet and
   --       look for columns that don't line up); it can also be sorted in
   --       a spreadsheet (for instance, to turn up rarely used record kinds).
   --
   -- An event trace file contains the following comma-separated fields:
   -- Event: UNKN (Unknown), CSTART (Compilation_Start),
   --        CEND (Compilation_End), CERR (Compile_Error),
   --        CWARN (Compile_Warning),
   --        BSTART (Binder_Start), BEND (Binder_End),
   --        BERR (Binder_Error), BWARN (Binder_Warning),
   --        EXSTART (Execution_Start),
   --        EXEND (Execution_End), EXFAIL (Execution_Failure),
   --        EXNA (Execution_Not_Applicable, EXSACT (Execution_Special_Action).
   --    (These values are case-insensitive.)
   --        In practice, "EVENT" should be treated as specifying a comment;
   --        this allows ignoring column headers even when they appear in the
   --        middle of files (which allows files to be concatenated without
   --        needing to remove the headers).
   -- Timestamp: The quoted timestamp, in the format specified by
   --       Ada.Calendar.Formatting.Image.
   -- Name: The quoted name of the source file, main subprogram, or test
   --       (as described in the record definition).
   -- Line: The Start_Line or Error_Line (if any). If not used, it can be
   --       omitted other than the comma separator.
   -- Position: The Error_Position (if any). If not used, it can be
   --       omitted other than the comma separator.
   -- Message:  The quoted message (as described in the record definition).
   --
   -- For example, the start of compiling test file B611001.A might look
   -- like:
   --
   -- CSTART, "2016-02-29 20:54:30.00", "B611001.A", 1,,
   --
   -- The first error in that file might look like (all on one line):
   --
   -- CERR, "2016-02-29 20:54:30.10", "B611001.A", 56, 12,
   --                                     "PRE is not allowed on an instance"

end Trace;
