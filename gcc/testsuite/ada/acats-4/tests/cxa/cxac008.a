-- CXAC008.A
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
-- OBJECTIVE:
--      Check that the Reset and Close subprograms defined in package
--      Ada.Streams.Stream_IO do not truncate Out_File mode files.
--
-- TEST DESCRIPTION:
--      Amendment 1 (AI95-00283-1) clarified that a stream file is not a
--      sequential file, and thus rules that apply only to sequential files
--      (like the latter part of A.8.2(16)) do not apply to them.
--
--      Many Ada 95 compilers did in fact treat stream files as sequential
--      files and thus truncated them during Close or Reset (or sometime Open).
--      This test is to check that the proper behavior for modern versions
--      of Ada is used.
--
--      Most of the subtests emulate an application that writes a data file,
--      then later makes a few in-place updates to the file. This can be done
--      with or without explicit positioning in the file. We also test using
--      Create to replace rather than update the data file.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations capable of supporting
--      external Stream_IO files.
--
--
-- CHANGE HISTORY:
--      27 Dec 01   RLB     Created test to survey behavior of Ada 95
--                          compilers.
--      19 Dec 14   RLB     Converted test to ACATS to reflect the resolutions
--                          of AI95-0283-1.
--      31 Dec 14   RLB     Corrected messages with the wrong subtest number.
--                          Corrected Subtest 8: wrong file size expected by
--                          code (compared to comments).
--      12 Mar 15   RLB     Fixed overlength lines.
--       3 Aug 15   RLB     Corrected another message with the wrong subtest
--                          number.

--!

with Ada.Streams.Stream_IO,
     Ada.Exceptions,
     Report;
procedure CXAC008 is
   Test_Filename : constant String :=
                              Report.Legal_File_Name ( Nam => "CXAC008" );
   use type Ada.Streams.Stream_IO.Count;
   Incomplete : exception;

   Initial_Data : constant Ada.Streams.Stream_Element_Array (1..20) :=
      (1 .. 5 => 86, 6 .. 9 => 25, 10 .. 20 => 4);


   procedure Check_File_Contents (
            Contents : in Ada.Streams.Stream_Element_Array;
            Descr : in String;
            Delete_It : in Boolean := True) is
      -- Check that the file (Test_Filename) contains Contents, and only
      -- Contents. If the check fails, include Descr in the message.
      File  : Ada.Streams.Stream_IO.File_Type;
      Check : Ada.Streams.Stream_Element_Array (Contents'Range);
      Last : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element;
   begin
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.In_File,
                                  Test_Filename);
      if Ada.Streams.Stream_IO.Size (File) /= Contents'Length then
          Report.Failed ("File size incorrect - " & Descr);
          Report.Comment ("File size was" &
                          Ada.Streams.Stream_IO.Count'Image(
                              Ada.Streams.Stream_IO.Size (File)) &
                          "; expected" & Ada.Streams.Stream_IO.Count'Image(
                              Contents'Length));
      else
         Ada.Streams.Stream_IO.Read (File, Check, Last);
         if Last /= Check'Last then
             Report.Failed ("Not enough elements read - " & Descr);
         elsif Check /= Contents then
             Report.Failed ("Contents incorrect - " & Descr);
             for I in Check'Range loop
                 if Check(I) /= Contents(I) then
                    Report.Comment ("Item " &
                      Ada.Streams.Stream_Element_Offset'Image(I) &
                      " differs: Contents=" &
                      Ada.Streams.Stream_Element'Image(Check(I)) &
                      "; ought to be=" &
                      Ada.Streams.Stream_Element'Image(Contents(I)));
                 end if;
             end loop;
         else
             -- Check that we cannot read past the expected size.
             begin
                Ada.Streams.Stream_IO.Read (File, Check, Last);
                if Last /= Check'First - 1 then
                    Report.Failed ("Able to read past end of file - " & Descr);
                end if;
             exception
                when Ada.Streams.Stream_IO.End_Error =>
                    Report.Failed ("End_Error raised by Stream_IO.Read - " &
                                   Descr);
             end;
         end if;
      end if;
      if Delete_It then
         Ada.Streams.Stream_IO.Delete (File); -- We're done with the file.
      else
         Ada.Streams.Stream_IO.Close (File);
      end if;
   end Check_File_Contents;


   procedure Create_File is
      -- Create a file containing 20 stream elements.
      File  : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create (File,                    -- Create.
                                    Ada.Streams.Stream_IO.Out_File,
                                    Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Initial_Data);
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (Initial_Data, "Initial", Delete_It => False);
   exception
       when Ada.Streams.Stream_IO.Use_Error |
            Ada.Streams.Stream_IO.Name_Error =>
          -- If an implementation does not support Stream_IO in a particular
          -- environment, the exception Use_Error or Name_Error will be raised
          -- on calls to Stream_IO.Create. If this happens, we handle these
          -- exceptions to produce a Not_Applicable result.
          Report.Not_Applicable (
              "Files not supported - Create as Out_File for Stream_IO");
          raise Incomplete;
   end Create_File;

begin

   Report.Test ("CXAC008", "Check that the Reset and Close subprograms " &
                           "defined in package Ada.Streams.Stream_IO do not " &
                           "truncate Out_File mode files");

   -- Basic case: Open the file as Out_File, write it sequentially (with fewer
   -- elements than it currently contains), and close it. It should not change
   -- size. (This subtest does not rely on positioning.)
   Subtest_1_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array(1..10) :=
        (1 .. 5 => 93, 6 .. 10 => 42);
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      Create_File; -- Checks for Not_Applicable.
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.Out_File,
                                  Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Data);
      File_Data(1..10) := Data;
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 1");
   end Subtest_1_Block;

   -- Basic case: Open the file as Out_File, write it sequentially (with fewer
   -- elements than it currently contains), and reset it to In_File, then
   -- close it. It should not change size. (This subtest does not rely on
   -- positioning.)
   Subtest_2_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array (1..8) :=
        (1 .. 5 => 90, 6 .. 8 => 36);
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.Out_File,
                                  Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Data);
      File_Data(1..8) := Data;
      Ada.Streams.Stream_IO.Reset (File, Ada.Streams.Stream_IO.In_File);
      if Ada.Streams.Stream_IO.Size (File) /= Initial_Data'Length then
          Report.Failed ("File size incorrect before close - Subtest 2");
          Report.Comment ("File size was" &
                          Ada.Streams.Stream_IO.Count'Image(
                              Ada.Streams.Stream_IO.Size (File)) &
                          "; expected" & Ada.Streams.Stream_IO.Count'Image(
                              Initial_Data'Length));
      end if;
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 2");
   end Subtest_2_Block;

   -- Basic case: Open the file as In_File, read some elements, reset it to
   -- Out_File, write it sequentially (with fewer elements than it currently
   -- contains), then close it. It should not change size. (This subtest
   -- does not rely on positioning.)
   Subtest_3_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Out_Data : constant Ada.Streams.Stream_Element_Array (1..7) :=
        (1 .. 4 => 94, 5 .. 7 => 21);
      In_Data : Ada.Streams.Stream_Element_Array (1..9);
      In_Last : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.In_File,
                                  Test_Filename);
      Ada.Streams.Stream_IO.Read  (File, In_Data, In_Last);
      if In_Last /= In_Data'Last then
          Report.Failed ("Not enough elements read - first read, Subtest 3");
      end if;
      Ada.Streams.Stream_IO.Reset (File, Ada.Streams.Stream_IO.Out_File);
      Ada.Streams.Stream_IO.Write (File, Out_Data);
      File_Data(1..7) := Out_Data;
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 3");
   end Subtest_3_Block;

   -- Basic case: Open the file as Out_File, write it sequentially (with fewer
   -- elements than it currently contains), change the mode to In_File,
   -- then close it. This should not change the file size.
   -- (This subtest does not rely on positioning.)
   Subtest_4_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array (1..9) :=
        (1 .. 5 => 56, 6 .. 9 => 37);
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      File_Data (Data'Range) := Data;
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.Out_File,
                                  Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Data);
      Ada.Streams.Stream_IO.Set_Mode (File, Ada.Streams.Stream_IO.In_File);
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 4");
   end Subtest_4_Block;

   -- Basic case: Create the file as Out_File, write it sequentially (with
   -- fewer elements than it currently contains), and close it. This SHOULD
   -- change the file size. (This subtest does not rely on positioning.)
   Subtest_5_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array(1..10) :=
        (1 .. 5 => 12, 6 .. 10 => 52);
   begin
      Create_File; -- Checks for Not_Applicable.
      Ada.Streams.Stream_IO.Create (File,
                                    Ada.Streams.Stream_IO.Out_File,
                                    Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Data);
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (Data, "Subtest 5");
   end Subtest_5_Block;

   -- Basic case: Create the file as Out_File, write it sequentially (with
   -- fewer elements than it currently contains), and reset it to In_File, then
   -- close it. This SHOULD change the file size. (This subtest does not rely
   -- on positioning.)
   Subtest_6_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array (1..8) :=
        (1 .. 5 => 90, 6 .. 8 => 36);
   begin
      Create_File;
      Ada.Streams.Stream_IO.Create (File,
                                    Ada.Streams.Stream_IO.Out_File,
                                    Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Data);
      Ada.Streams.Stream_IO.Reset (File, Ada.Streams.Stream_IO.In_File);
      if Ada.Streams.Stream_IO.Size (File) /= Data'Length then
          Report.Failed ("File size incorrect before close - Subtest 6");
          Report.Comment ("File size was" &
                          Ada.Streams.Stream_IO.Count'Image(
                              Ada.Streams.Stream_IO.Size (File)) &
                          "; expected" & Ada.Streams.Stream_IO.Count'Image(
                              Data'Length));
      end if;
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (Data, "Subtest 6");
   end Subtest_6_Block;


   -- Basic Positioning case: Open the file as Out_File, position it, write a
   -- couple of elements, and close the file. This should not change the file
   -- size.
   Subtest_7_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array (1..2) :=
        (1 .. 2 => 4);
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      File_Data (8 .. 9) := Data;
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.Out_File,
                                  Test_Filename);
      begin
         Ada.Streams.Stream_IO.Set_Index (File, 8);
      exception
        when Ada.Streams.Stream_IO.Use_Error =>
          -- If an implementation does not support positioning on a normal
          -- stream file, the exception Use_Error will be raised
          -- on calls to Stream_IO.Set_Index. If this happens, we stop the
          -- test at this point with an appropriate comment.
          Report.Comment (
              "Positioning not supported on " & Test_Filename &
              " with Out_File mode; positioning tests omitted");
          raise Incomplete;
      end;
      Ada.Streams.Stream_IO.Write (File, Data);
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 7");
   end Subtest_7_Block;

   -- Basic Positioning case: Open the file as Out_File, write a couple of
   -- elements at a middle position, reset it to In_File, and close the file.
   -- This should not change the file size.
   Subtest_8_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array (1..2) :=
        (1 .. 2 => 4);
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      File_Data (11 .. 12) := Data;
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.Out_File,
                                  Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Data, To => 11);
      Ada.Streams.Stream_IO.Reset (File, Ada.Streams.Stream_IO.In_File);
      if Ada.Streams.Stream_IO.Size (File) /= Initial_Data'Length then
          Report.Failed ("File size incorrect before close - Subtest 8");
          Report.Comment ("File size was" &
                          Ada.Streams.Stream_IO.Count'Image(
                              Ada.Streams.Stream_IO.Size (File)) &
                          "; expected" & Ada.Streams.Stream_IO.Count'Image(
                              Initial_Data'Length));
      end if;
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 8");
   end Subtest_8_Block;

   -- Basic Positioning case: Open the file as In_File, read a few elements,
   -- reset it to Out_File, write a couple of elements at a middle position,
   -- then close the file. This should not change the file size.
   Subtest_9_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Out_Data : constant Ada.Streams.Stream_Element_Array (1..3) :=
        (1 .. 3 => 4);
      In_Data : Ada.Streams.Stream_Element_Array (1..4);
      In_Last : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      File_Data (4 .. 6) := Out_Data;
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.In_File,
                                  Test_Filename);
      Ada.Streams.Stream_IO.Read  (File, In_Data, In_Last, From => 10);
      if In_Last /= In_Data'Last then
          Report.Failed ("Not enough elements read - first read, Subtest 9");
      end if;
      Ada.Streams.Stream_IO.Reset (File, Ada.Streams.Stream_IO.Out_File);
      Ada.Streams.Stream_IO.Write (File, Out_Data, To => 4);
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 9");
   end Subtest_9_Block;

   -- Basic Positioning case: Open the file as Out_File, write a few
   -- elements at a middle position, change the mode to In_File, and
   -- close the file. This should not change the file size.
   Subtest_10_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array (1..3) :=
        (1 .. 3 => 30);
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      File_Data (8 .. 10) := Data;
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.Out_File,
                                  Test_Filename);
      Ada.Streams.Stream_IO.Write (File, Data, To => 8);
      Ada.Streams.Stream_IO.Set_Mode (File, Ada.Streams.Stream_IO.In_File);
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 10");
   end Subtest_10_Block;

   -- Positioning case: Open the file as Append_File, position it, write a
   -- couple of elements, and close the file. This should not change the file
   -- size. We check this last because an implementation can support
   -- positioning on Out_File mode files, and not support it on Append_File
   -- mode files. Also, truncation of Append_File is not interesting if
   -- positioning is not supported (it would have to be at the last location
   -- in the file).
   Subtest_11_Block:
   declare
      File  : Ada.Streams.Stream_IO.File_Type;
      Data : constant Ada.Streams.Stream_Element_Array (1..2) :=
        (1 .. 2 => 4);
      File_Data : Ada.Streams.Stream_Element_Array := Initial_Data;
   begin
      File_Data (5 .. 6) := Data;
      Create_File;
      Ada.Streams.Stream_IO.Open (File,
                                  Ada.Streams.Stream_IO.Append_File,
                                  Test_Filename);
      begin
         Ada.Streams.Stream_IO.Set_Index (File, 5);
      exception
        when Ada.Streams.Stream_IO.Use_Error =>
          -- If an implementation does not support positioning on an append
          -- mode stream file, the exception Use_Error will be raised
          -- on calls to Stream_IO.Set_Index. If this happens, we stop the
          -- test at this point with an appropriate comment.
          Report.Comment (
              "Positioning not supported on " & Test_Filename &
              " with Append_File mode; Append_File positioning tests omitted");
          raise Incomplete;
      end;
      Ada.Streams.Stream_IO.Write (File, Data);
      Ada.Streams.Stream_IO.Close (File);
      Check_File_Contents (File_Data, "Subtest 11");
   end Subtest_11_Block;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when Info:others     =>
      Report.Failed ("Unexpected exception - " &
                     Ada.Exceptions.Exception_Information(Info));
      Report.Result;

end CXAC008;
