-- CXAC006.A
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
--    Check that stream files opened in Append_Mode handle positioning
--    properly or not at all. (Also, check the other questions of AI95-00085.)
--
-- CHANGE HISTORY:
--    12 Jan 2001   RLB   Initial version for AI85 resolution.
--    21 Aug 2007   RLB   Converted into an ACATS test using the final
--                        resolution of AI95-00085.
--    19 May 2014   RLB   Added missing name to Legal_File_Name (default
--                        is nonsense as it is called before Test).
--
--!
with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Report;
procedure CXAC006 is
    -- Make a shorter name for the tested package:
    package Strm_IO renames Ada.Streams.Stream_IO;

    -- Test data:

    File_Contents : array (Strm_IO.Count range 1 .. 40) of Character;
    File_Length : Strm_IO.Count := 0;
    File_Name : constant String := Report.Legal_File_Name (Nam => "CXAC006");
	-- A file name that will be used for the test file. This file will
	-- be written, and should be positionable, if possible.

    File : Strm_IO.File_Type;
    Another_File : Strm_IO.File_Type;
    use type Strm_IO.Count;

    -- Test subprograms:
    Abort_Test : exception; -- Some test invariant failed.

    procedure Write_at_Index (File : in Strm_IO.File_Type;
			      Index : in Strm_IO.Count;
			      Data : in Character;
			      Positioning_OK : in Boolean := False) is
	-- Write an item at Index, updating the local copy of the data as well.
	-- Any file positioning has already been accomplished. Positioning of
	-- the file is OK if Positioning_OK is True.
    begin
	if Positioning_OK then
	    if Strm_IO.Index (File) /= Index then
		Report.Failed ("File index does not match test index - Index=" &
		     Strm_IO.Count'Image(Index) & " File Index=" &
		     Strm_IO.Count'Image(Strm_IO.Index (File)));
                -- We allow the test to continue here, because some
                -- implementations have a broken implementation of Index.
	    end if;
	end if;
	Character'Write (Strm_IO.Stream(File), Data);
	if Index > File_Length then
	    File_Length := Index;
	end if;
	File_Contents(Index) := Data;
    end Write_at_Index;


    procedure Check_File_Contents (File : in out Strm_IO.File_Type;
				   Code : in Character) is
	-- Check the file contents against the local set. File will have
	-- mode "In_File" after this check.
	Data : Character;
	Any_Errors : Boolean := False;
    begin
	begin
	    Strm_IO.Reset (File => File,
			   Mode => Strm_IO.In_File);
	exception
	    when Info:others =>
		Report.Failed ('(' & Code & ") Unable to reset stream file - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		raise Abort_Test;
	end;

	for I in 1 .. File_Length loop
	    Character'Read (Strm_IO.Stream(File), Data);
	    if Data /= File_Contents(I) then
		Report.Failed ('(' & Code & ") File contents mismatch at " &
			Strm_IO.Count'Image(I) & "Read='" & Data &
			"', expected='" & File_Contents(I) & ''');
		Any_Errors := True;
	    end if;
	end loop;
	if Code in '1' .. '9' and (not Any_Errors) then
	    Report.Comment ('(' & Code & ") File contents as expected");
	end if;
	begin
	    if File_Length /= Strm_IO.Size (File) then
		Report.Failed ('(' & Code & ") File size mismatch " &
			"Read=" & Strm_IO.Count'Image(Strm_IO.Size (File)) &
			", expected=" & Strm_IO.Count'Image(File_Length));
		Any_Errors := True;
	    end if;
	exception
	    when Strm_IO.Use_Error => null; -- Probably not positionable.
	end;
    exception
	when Strm_IO.End_Error =>
	    Report.Failed ('(' & Code & ") Read past end of file when " &
		"checking contents");
	when Info:others =>
	    Report.Failed ('(' & Code & ") Unable to read stream file - " &
		Ada.Exceptions.Exception_Name(Info));
	    Report.Comment ("Additional information: " &
		Ada.Exceptions.Exception_Information(Info));
	    raise Abort_Test;
    end Check_File_Contents;

begin
    Report.Test ("CXAC006", "Check that stream files opened in Append_Mode handle " &
                 "positioning properly or not at all");
    begin
	Strm_IO.Create (File => File,
	        Mode => Strm_IO.Out_File,
	        Name => File_Name);
	Strm_IO.Delete (File);
    exception
	when Strm_IO.Name_Error =>
	    Report.Not_Applicable ("Unable to create stream file - Name_Error");
	    Report.Comment ("File_Name is " & File_Name);
	    raise Abort_Test;
	when Strm_IO.Use_Error =>
	    Report.Not_Applicable ("Unable to create stream file - Use_Error");
	    Report.Comment ("File_Name is " & File_Name);
	    raise Abort_Test;
	when Info:others =>
	    Report.Not_Applicable ("Unable to create stream file - " &
		Ada.Exceptions.Exception_Name(Info));
	    Report.Comment ("Additional information: " &
		Ada.Exceptions.Exception_Information(Info));
	    raise Abort_Test;
    end;

    declare
	-- Test information:
	Current_Position : Strm_IO.Count;
	Append_Supports_Positioning : Boolean;
	Out_Supports_Positioning : Boolean;
	In_Supports_Positioning : Boolean;
    begin
	-- OK, we can create a file. Let's start the test:
	begin
	    Strm_IO.Create (File => File,
			   Mode => Strm_IO.Append_File,
			   Name => File_Name);
	exception
	    when Strm_IO.Name_Error =>
		Report.Not_Applicable ("Unable to create append stream file - " &
			"Name_Error");
        Report.Comment ("File_Name is " & File_Name);
		raise Abort_Test;
	    when Strm_IO.Use_Error =>
		Report.Not_Applicable ("Unable to create append stream file - " &
			"Use_Error");
        Report.Comment ("File_Name is " & File_Name);
		raise Abort_Test;
	    when Info:others =>
		Report.Not_Applicable ("Unable to create append stream file - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		raise Abort_Test;
	end;

	-- Write a few characters:
	Write_at_Index (File, 1, 'R');
	Write_at_Index (File, 2, 'L');
	Write_at_Index (File, 3, 'B');
	Write_at_Index (File, 4, ' ');
	Write_at_Index (File, 5, ' ');
	Current_Position := 6;

	-- Determine if the append file is positionable:
	begin
	    if Strm_IO.Index (File) /= Current_Position then
		Report.Failed ("(1) Wrong position for Append_File mode - position =" &
		     Strm_IO.Count'Image(Strm_IO.Index (File)));
	    end if;
	    Report.Comment ("(1) Stream file in Append_File mode is positionable");
	    Append_Supports_Positioning := True;
	exception
	    when Strm_IO.Use_Error =>
		Report.Comment ("(1) Stream file in Append_File mode is NOT" &
			" positionable");
		Append_Supports_Positioning := False;
	    when Info:others =>
		Report.Failed ("(1) Unexpected exception - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		Append_Supports_Positioning := False;
	end;

	-- Simulate backing up to make a correction:
	begin
	    Strm_IO.Set_Index (File, 5);
	    Current_Position := 5;
	    if not Append_Supports_Positioning then
		Report.Comment ("(2) Weird: Append_File mode supports Set_Index, " &
			"but not Index");
	    else
		Report.Comment ("(2) Expected: Both Set_Index and Index supported");
	    end if;
	exception
	    when Strm_IO.Use_Error =>
		if Append_Supports_Positioning then
		    Report.Failed ("(2) Append_File mode supports Index, but not " &
			    "Set_Index");
		else
		    Report.Comment ("(2) Expected: Both Set_Index and Index " &
			     "supported");
		end if;
	end;
	if Append_Supports_Positioning then
	    if Strm_IO.Index (File) /= Current_Position then
		Report.Failed ("(2A) Set_Index accepted, but position unchanged - position =" &
		     Strm_IO.Count'Image(Strm_IO.Index (File)));
		Current_Position := Strm_IO.Index (File);
	    end if;
	end if;

	-- Continue writing data:
	if Append_Supports_Positioning and
	   Current_Position = 5 then
	    -- We don't intend to write at the end of the file; check if the
	    -- implementation is using O_APPEND and writes at the end anyway.
	    if Strm_IO.Size (File) /= File_Length or else
		File_Length /= 5 then
		Report.Failed ("(2B) File Size incorrect after writes");
	    end if;
	    Write_at_Index (File, Current_Position,     'S',
			    Positioning_OK => True);
	    if Strm_IO.Size (File) /= File_Length then
		Report.Failed ("(2C) Set_Index accepted, but file written at end anyway");
		Current_Position := Strm_IO.Size (File);
		-- Repair the file data to reflect what really happened.
		File_Contents(5) := ' ';
		File_Contents(6) := 'S';
		File_Length := 6;
	    -- else OK.
	    end if;
	else
	    Write_at_Index (File, Current_Position,     'S',
			    Positioning_OK => Append_Supports_Positioning);
	end if;
	Write_at_Index (File, Current_Position + 1, 'T',
			Positioning_OK => Append_Supports_Positioning);
	Write_at_Index (File, Current_Position + 2, 'T',
			Positioning_OK => Append_Supports_Positioning);
	Write_at_Index (File, Current_Position + 3, ' ',
			Positioning_OK => Append_Supports_Positioning);
	Current_Position := Current_Position + 4;

	-- OK, now check results. (This also checks resetting the mode to
	-- In_File).
	Check_File_Contents (File, 'A');

	-- Check whether we can position in this mode.
	begin
	    Strm_IO.Set_Index (File, 6);
	    Report.Comment ("(A) In_File mode supports positioning");
	    In_Supports_Positioning := True;
	exception
	    when Strm_IO.Use_Error =>
        Report.Comment ("(A) In_File mode does not support positioning");
		In_Supports_Positioning := False;
	    when Info:others =>
		Report.Failed ("(A) Unexpected exception - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		In_Supports_Positioning := False;
	end;

	-- Change the mode back to Append_File:
	Strm_IO.Set_Mode (File, Strm_IO.Append_File);
	Current_Position := File_Length + 1;
	if Append_Supports_Positioning then
	    if Strm_IO.Index (File) /= Current_Position then
		Current_Position := Strm_IO.Index (File);
		Report.Failed ("(B) Wrong position for Set_Mode Append_File - " &
		     "position =" & Strm_IO.Count'Image(Current_Position));
	    end if;
	end if;

	-- Write some more:
	Write_at_Index (File, Current_Position,     'N',
			Positioning_OK => Append_Supports_Positioning);
	Write_at_Index (File, Current_Position + 1, 'H',
			Positioning_OK => Append_Supports_Positioning);
	Write_at_Index (File, Current_Position + 2, 'C',
			Positioning_OK => Append_Supports_Positioning);
	Write_at_Index (File, Current_Position + 3, ' ',
			Positioning_OK => Append_Supports_Positioning);
	Current_Position := Current_Position + 4;

	-- Try setting the mode to Out_File:
	begin
	    Strm_IO.Set_Mode (File, Strm_IO.Out_File);
	    -- Check that Set_Mode doesn't change the position:
	    begin
		if Strm_IO.Index (File) /= Current_Position then
		    Report.Failed ("(C) Set_Mode (Out_File) changed the file " &
			"position to " &
			Strm_IO.Count'Image(Strm_IO.Index (File)));
		end if;
	    exception
		when Strm_IO.Use_Error =>
			null; -- Not positionable (checked below).
	    end;
	exception
	    when Info:others =>
		Report.Comment ("(C) Cannot Set_Mode to Out_File - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		raise Abort_Test;
	end;

	-- Check whether we can position in this mode:
	begin
	    Strm_IO.Set_Index (File, 2);
	    Current_Position := 2;
	    Report.Comment ("(4) Out_File supports positioning");
	    Out_Supports_Positioning := True;
	exception
	    when Strm_IO.Use_Error =>
        Report.Comment ("(4) Out_File does not support positioning");
		Out_Supports_Positioning := False;
	end;

	-- Write a few more characters:
	Write_at_Index (File, Current_Position,     'K',
			Positioning_OK => Out_Supports_Positioning);
	Write_at_Index (File, Current_Position + 1, 'B',
			Positioning_OK => Out_Supports_Positioning);
	Write_at_Index (File, Current_Position + 2, 'D',
			Positioning_OK => Out_Supports_Positioning);
	Current_Position := Current_Position + 3;

	-- If we can position, check the Set_Mode (In_File) doesn't change the
	-- current pointer:
	begin
	    Strm_IO.Set_Mode (File, Strm_IO.In_File);
	    -- Check that Set_Mode doesn't change the position:
	    begin
		if Strm_IO.Index (File) /= Current_Position then
		    Report.Failed ("(D) Set_Mode (In_File) changed the file " &
			"position to " &
			Strm_IO.Count'Image(Strm_IO.Index (File)));
		end if;
	    exception
		when Strm_IO.Use_Error =>
		    null; -- Not positionable.
	    end;
	exception
	    when Info:others =>
		Report.Comment ("(D) Cannot Set_Mode to In_File - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
	end;

	-- OK, now check results.
	Check_File_Contents (File, 'E');

	Strm_IO.Close (File);

	-- Re-open file (with a different file object) and check
	-- contents again:
	begin
	    Strm_IO.Open (File => Another_File,
			  Mode => Strm_IO.In_File,
			  Name => File_Name);
	exception
	    when Info:others =>
		Report.Failed ("(F) Unable to reopen append stream file - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		raise Abort_Test;
	end;
	Check_File_Contents (Another_File, '5');
	    -- Note: This checks AI-85 question 2: does Reset start at the
	    -- beginning of the external file?

	begin
	    Strm_IO.Set_Index (Another_File, 6);
	    Report.Comment ("(6) In_File on reopened file supports positioning");
	    In_Supports_Positioning := True;
	exception
	    when Strm_IO.Use_Error =>
        Report.Comment ("(6) In_File on reopened file does not support positioning");
		In_Supports_Positioning := False;
	    when Info:others =>
		Report.Failed ("(6) Unexpected exception - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		In_Supports_Positioning := False;
	end;

	-- Reset mode to Append_File:
	begin
	    Strm_IO.Set_Mode (File => Another_File,
			      Mode => Strm_IO.Append_File);
	exception
	    when Info:others =>
		Report.Comment ("(H) Unable to Set_Mode from In_File to " &
			"Append_File - exception - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
		raise Abort_Test;
	end;

	-- Check that Set_Mode to the current mode works (question 4 of AI-85).
	begin
	    Strm_IO.Set_Mode (File => Another_File,
			      Mode => Strm_IO.Append_File);
	    begin
		if Strm_IO.Index (Another_File) /= File_Length + 1 then
		    Report.Failed ("(8) Wrong position for Set_Mode(Append_File) - " &
			 "position =" &
			 Strm_IO.Count'Image(Strm_IO.Index (Another_File)));
		else
		    Report.Comment ("(8) Set_Mode(Append_File) works");
		end if;
	    exception
		when Strm_IO.Use_Error =>
		    if Append_Supports_Positioning then
			Report.Failed ("(8) Use_Error checking position of " &
				 "Set_Mode(Append_File)");
		    else
			Report.Comment ("(8) Unable to check position of " &
				 "Set_Mode(Append_File) - no positioning");
		    end if;
	    end;
	exception
	    when Info:others =>
		Report.Comment ("(8) Unable to Set_Mode from Append_File to " &
			"Append_File - exception - " &
			Ada.Exceptions.Exception_Name(Info));
		Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
	end;

	Strm_IO.Delete (Another_File); -- Clean up after ourselves.

    exception
	when Abort_Test =>
	    Report.Comment ("Test aborted");
	    if Strm_IO.Is_Open (File) then
		Strm_IO.Delete (File);
	    end if;
	    if Strm_IO.Is_Open (Another_File) then
		Strm_IO.Delete (Another_File);
	    end if;
	when Info:others =>
	    Report.Failed ("Unexpected exception - " &
			Ada.Exceptions.Exception_Name(Info));
	    Report.Comment ("  Additional information: " &
			Ada.Exceptions.Exception_Information(Info));
	    if Strm_IO.Is_Open (File) then
		Strm_IO.Delete (File);
	    end if;
	    if Strm_IO.Is_Open (Another_File) then
		Strm_IO.Delete (Another_File);
	    end if;
    end;

    Report.Result;
end CXAC006;
