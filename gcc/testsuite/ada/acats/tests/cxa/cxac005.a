-- CXAC005.A
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
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--    Check that stream file positioning work as specified.  (Defect Report
--    8652/0055).
--
-- CHANGE HISTORY:
--    12 FEB 2001   PHL   Initial version.
--    14 MAR 2001   RLB   Readied for release; fixed Not_Applicable check
--                        to terminate test gracefully.
--    05 MAR 2007   RLB   Updated to avoid problems with return-by-reference.
--
--!
with Ada.Streams.Stream_Io;
use Ada.Streams;
with Ada.Exceptions;
use Ada.Exceptions;
with Report;
use Report;
procedure CXAC005 is

    Incomplete : exception;

    procedure TC_Assert (Condition : Boolean; Message : String) is
    begin
        if not Condition then
            Failed (Message);
        end if;
    end TC_Assert;

    package Checked_Stream_Io is

        type File_Type (Max_Size : Stream_Element_Count) is limited private;

        procedure Create (File : in out File_Type;
                          Mode : in Stream_Io.File_Mode := Stream_Io.Out_File;
                          Name : in String := "";
                          Form : in String := "");

        procedure Open (File : in out File_Type;
                        Mode : in Stream_Io.File_Mode;
                        Name : in String;
                        Form : in String := "");

        procedure Close (File : in out File_Type);
        procedure Delete (File : in out File_Type);

        procedure Reset (File : in out File_Type;
                         Mode : in Stream_Io.File_Mode);
        procedure Reset (File : in out File_Type);

        procedure Read (File : in out File_Type;
                        Item : out Stream_Element_Array;
                        Last : out Stream_Element_Offset;
                        From : in Stream_Io.Positive_Count);

        procedure Read (File : in out File_Type;
                        Item : out Stream_Element_Array;
                        Last : out Stream_Element_Offset);

        procedure Write (File : in out File_Type;
                         Item : in Stream_Element_Array;
                         To : in Stream_Io.Positive_Count);

        procedure Write (File : in out File_Type;
                         Item : in Stream_Element_Array);

        procedure Set_Index (File : in out File_Type;
                             To : in Stream_Io.Positive_Count);

        function Index (File : in File_Type) return Stream_Io.Positive_Count;

        function Size (File : in File_Type) return Stream_Io.Count;

        procedure Set_Mode (File : in out File_Type;
                            Mode : in Stream_Io.File_Mode);

    private
        type File_Type (Max_Size : Stream_Element_Count) is
            record
                File : Stream_Io.File_Type;
                Index : Stream_Io.Positive_Count;
                Contents :
                   Stream_Element_Array
                      (Stream_Element_Offset (Ident_Int (1)) .. Max_Size);
            end record;
    end Checked_Stream_Io;

    package body Checked_Stream_Io is

        use Stream_Io;

        procedure Create (File : in out File_Type;
                          Mode : in Stream_Io.File_Mode := Stream_Io.Out_File;
                          Name : in String := "";
                          Form : in String := "") is
        begin
            Stream_Io.Create (File.File, Mode, Name, Form);
            File.Index := Stream_Io.Index (File.File);
            if Mode = Append_File then
                TC_Assert (File.Index = Stream_Io.Size (File.File) + 1,
                        "Index /= Size + 1 -- Create - Append_File");
            else
                TC_Assert (File.Index = 1, "Index /= 1 -- Create - " &
                                           File_Mode'Image (Mode));
            end if;
        end Create;

        procedure Open (File : in out File_Type;
                        Mode : in Stream_Io.File_Mode;
                        Name : in String;
                        Form : in String := "") is
        begin
            Stream_Io.Open (File.File, Mode, Name, Form);
            File.Index := Stream_Io.Index (File.File);
            if Mode = Append_File then
                TC_Assert (File.Index = Stream_Io.Size (File.File) + 1,
                        "Index /= Size + 1 -- Open - Append_File");
            else
                TC_Assert (File.Index = 1, "Index /= 1 -- Open - " &
                                           File_Mode'Image (Mode));
            end if;
        end Open;

        procedure Close (File : in out File_Type) is
        begin
            Stream_Io.Close (File.File);
        end Close;

        procedure Delete (File : in out File_Type) is
        begin
            Stream_Io.Delete (File.File);
        end Delete;

        procedure Reset (File : in out File_Type;
                         Mode : in Stream_Io.File_Mode) is
        begin
            Stream_Io.Reset (File.File, Mode);
            File.Index := Stream_Io.Index (File.File);
            if Mode = Append_File then
                TC_Assert (File.Index = Stream_Io.Size (File.File) + 1,
                        "Index /= Size + 1 -- Reset - Append_File");
            else
                TC_Assert (File.Index = 1, "Index /= 1 -- Reset - " &
                                           File_Mode'Image (Mode));
            end if;
        end Reset;

        procedure Reset (File : in out File_Type) is
        begin
            Reset (File, Stream_Io.Mode (File.File));
        end Reset;


        procedure Read (File : in out File_Type;
                        Item : out Stream_Element_Array;
                        Last : out Stream_Element_Offset;
                        From : in Stream_Io.Positive_Count) is
        begin
            Set_Index (File, From);
            Read (File, Item, Last);
        end Read;

        procedure Read (File : in out File_Type;
                        Item : out Stream_Element_Array;
                        Last : out Stream_Element_Offset) is
            Index : constant Stream_Element_Offset :=
               Stream_Element_Offset (File.Index);
        begin
            Stream_Io.Read (File.File, Item, Last);
            if Last < Item'Last then
                TC_Assert (Item (Item'First .. Last) =
                        File.Contents (Index .. Index + Last - Item'First),
                        "Incorrect data read from file - 1");
                TC_Assert (Count (Index + Last - Item'First) =
                        Stream_Io.Size (File.File),
                        "Read stopped before end of file");
                File.Index := Count (Index + Last - Item'First) + 1;
            else
                TC_Assert (Item = File.Contents (Index .. Index + Item'Length - 1),
                        "Incorrect data read from file - 2");
                File.Index := File.Index + Item'Length;
            end if;
        end Read;

        procedure Write (File : in out File_Type;
                         Item : in Stream_Element_Array;
                         To : in Stream_Io.Positive_Count) is
        begin
            Set_Index (File, To);
            Write (File, Item);
        end Write;

        procedure Write (File : in out File_Type;
                         Item : in Stream_Element_Array) is
            Index : constant Stream_Element_Offset :=
               Stream_Element_Offset (File.Index);
        begin
            Stream_Io.Write (File.File, Item);
            File.Contents (Index .. Index + Item'Length - 1) := Item;
            File.Index := File.Index + Item'Length;
            TC_Assert (File.Index = Stream_Io.Index (File.File),
                    "Write failed to move the index");
        end Write;

        procedure Set_Index (File : in out File_Type;
                             To : in Stream_Io.Positive_Count) is
        begin
            Stream_Io.Set_Index (File.File, To);
            File.Index := Stream_Io.Index (File.File);
            TC_Assert (File.Index = To, "Set_Index failed");
        end Set_Index;

        function Index (File : in File_Type) return Stream_Io.Positive_Count is
            New_Index : constant Count := Stream_Io.Index (File.File);
        begin
            TC_Assert (New_Index = File.Index, "Index changed unexpectedly");
            return New_Index;
        end Index;

        function Size (File : in File_Type) return Stream_Io.Count is
            New_Size : constant Count := Stream_Io.Size (File.File);
        begin
            TC_Assert (New_Size <= Count(File.Max_Size), "File too large");
            return New_Size;
        end Size;

        procedure Set_Mode (File : in out File_Type;
                            Mode : in Stream_Io.File_Mode) is
            Old_Index : constant Count := File.Index;
        begin
            Stream_Io.Set_Mode (File.File, Mode);
            File.Index := Stream_Io.Index (File.File);
            if Mode = Append_File then
                TC_Assert (File.Index = Stream_Io.Size (File.File) + 1,
                        "Index /= Size + 1 -- Set_Mode - Append_File");
            else
                TC_Assert (File.Index = Old_Index, "Set_Mode changed the index");
            end if;
        end Set_Mode;

    end Checked_Stream_Io;

    package Csio renames Checked_Stream_Io;

    F : Csio.File_Type (100);
    S : Stream_Element_Array (1 .. 10);
    Last : Stream_Element_Offset;

begin

    Report.Test ("CXAC005",
                 "Check that stream file positioning work as specified");

    declare
        Name : constant String := Legal_File_Name;
    begin
        begin
            Csio.Create (F, Name => Name);
        exception
            when others =>
                Not_Applicable ("Files not supported - Creation with Out_File for Stream_IO");
                raise Incomplete;
        end;

        for I in Stream_Element range 1 .. 10 loop
            Csio.Write (F, ((1 => I + 2)));
        end loop;
        Csio.Write (F, (1 .. 15 => 11));
        Csio.Write (F, (1 .. 15 => 12), To => 15);

        Csio.Reset (F);

        for I in Stream_Element range 1 .. 10 loop
            Csio.Write (F, (1 => I));
        end loop;
        Csio.Write (F, (1 .. 15 => 13));
        Csio.Write (F, (1 .. 15 => 14), To => 15);
        Csio.Write (F, (1 => 90));

        Csio.Set_Mode (F, Stream_Io.In_File);

        Csio.Read (F, S, Last);
        Csio.Read (F, S, Last, From => 3);
        Csio.Read (F, S, Last, From => 28);

        Csio.Set_Mode (F, Stream_Io.Append_File);
        Csio.Write (F, (1 .. 5 => 88));

        Csio.Close (F);

        Csio.Open (F, Name => Name, Mode => Stream_Io.Append_File);
        Csio.Write (F, (1 .. 3 => 33));

        Csio.Set_Mode (F, Stream_Io.In_File);
        Csio.Read (F, S, Last, From => 20);
        Csio.Read (F, S, Last);
        Csio.Reset (F, Stream_Io.Out_File);

        Csio.Write (F, (1 .. 9 => 99));

        -- Check the contents of the entire file.
        declare
            S : Stream_Element_Array
                   (1 .. Stream_Element_Offset (Csio.Size (F)));
        begin
            Csio.Reset (F, Stream_Io.In_File);
            Csio.Read (F, S, Last);
        end;

        Csio.Delete (F);
    end;

    Report.Result;
exception
   when Incomplete =>
      Report.Result;
   when E:others     =>
      Report.Failed ("Unexpected exception raised - " & Exception_Name (E) &
                      " - " & Exception_Message (E));
      Report.Result;

end CXAC005;

