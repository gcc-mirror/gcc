-- CXA4036.A
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
--*
--
-- OBJECTIVE:
--      Check encoding, decoding and conversions between regular strings
--      and UTF_* encodings. Case 1: Standard.String

--
-- TEST DESCRIPTION:
--      This test
--
-- CHANGE HISTORY:
--      10 SEP 13   JPR     Created test.
--      16 OCT 13   RLB     Fixed header.
--       4 APR 14   RLB     Renamed test to create ACATS 4.0 version.
--                          Added failures to reflect the resolution
--                          of AI12-0088-1 (which is a ramification, not
--                          a change).
--
--!

with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.UTF_Encoding.Strings;

with Report;
with Ada.Exceptions;
procedure CXA4036 is
   subtype Hex_Int  is Integer range 0..15;
   subtype Hex_Char is Character with
     Static_Predicate => Hex_Char in ' '| '0'..'9' | 'A' .. 'F' | 'a' .. 'f';
   type    Hex_String is array (Positive range <>) of Hex_Char;

   function To_String (Hexa : Hex_String) return String is
      -- Returns a string whose code points are given in textual
      -- hexadecimal. Allows a single space between pairs of hex digits
      function Value (C : Hex_Char) return Hex_Int is
	 (case C is
	    when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
	    when 'A' .. 'F' => Character'Pos (C) - Character'Pos ('A') + 10,
	    when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
	    when ' '        => raise Constraint_Error
	 );

      Result : String (1 .. Hexa'Length/2);
      H : Natural := Hexa'First;
   begin
      for R in Result'Range loop
	 Result (R) := Character'Val (Value (Hexa (H))*16 + Value (Hexa (H+1)));
	 H := H + 2;
	 if H <= Hexa'Last and then Hexa (H) = ' ' then
	    H := H + 1;
	 end if;
	 if H > Hexa'Last then
	    return Result (1..R);
	 elsif H = Hexa'Last then -- Missing digit
	    raise Constraint_Error;
	 end if;
      end loop;
      return Result;
   end To_String;

   use Ada.Strings.UTF_Encoding, Ada.Strings.UTF_Encoding.Conversions,
       Ada.Strings.UTF_Encoding.Strings;

   use Ada.Exceptions;

   -- A string with various characters from the upper half of Latin-1:
   Test_String : constant String := To_String ("A1 A2 B3 B4 C5 C6 D7 D8 E9 EA FB FC");
begin
   Report.Test ("CXA4036",
                "Check encoding, decoding and conversions between regular " &
                "strings and UTF_* encodings. Case 1: Standard.String");

   -- Check constants
   if BOM_8 /= To_String ("EF BB BF") then
      Report.Failed ("Invalid value for BOM_8");
   end if;
   if BOM_16BE /= To_String ("FE FF") then
      Report.Failed ("Invalid value for BOM_16BE");
   end if;
   if BOM_16LE /= To_String ("FF FE") then
      Report.Failed ("Invalid value for BOM_16LE");
   end if;
   if Wide_Character'Pos (BOM_16 (1)) /= 16#FEFF# then
      Report.Failed ("Invalid value for BOM_16");
   end if;

   -- Check regular encodings/decodings
   if Encode (Test_String, UTF_8) /= To_String ("C2A1 C2A2 C2B3 C2B4 C385 C386 C397 C398 C3A9 C3AA C3BB C3BC") then
      Report.Failed ("Wrong result of Encode for UTF_8");
   end if;
   if Encode (Test_String, UTF_16BE) /= To_String ("00A1 00A2 00B3 00B4 00C5 00C6 00D7 00D8 00E9 00EA 00FB 00FC") then
      Report.Failed ("Wrong result of Encode for UTF_16BE");
   end if;
   if Encode (Test_String, UTF_16LE) /= To_String ("A100 A200 B300 B400 C500 C600 D700 D800 E900 EA00 FB00 FC00") then
      Report.Failed ("Wrong result of Encode for UTF_16LE");
   end if;
   if Decode (To_String ("C2A1 C2A2 C2B3 C2B4 C385 C386 C397 C398 C3A9 C3AA C3BB C3BC"), UTF_8) /= Test_String then
      Report.Failed ("Wrong result of Decode for UTF_8");
   end if;
   if Decode (To_String ("00A1 00A2 00B3 00B4 00C5 00C6 00D7 00D8 00E9 00EA 00FB 00FC"), UTF_16BE) /= Test_String then
      Report.Failed ("Wrong result of Decode for UTF_16BE");
   end if;
   if Decode (To_String ("A100 A200 B300 B400 C500 C600 D700 D800 E900 EA00 FB00 FC00"), UTF_16LE) /= Test_String then
      Report.Failed ("Wrong result of Decode for UTF_16LE");
   end if;


   -- Check BOM is recognized
   if Encoding (BOM_8 & "Hello") /= UTF_8 or  -- A string with BOM
     Encoding (BOM_8) /= UTF_8 or             -- A string containing only BOM
     Encoding ("")   /= UTF_8  or             -- An empty string (default encoding)
     Encoding ("ABCS", UTF_16BE) /= UTF_16BE  -- A string without BOM and different default
   then
      Report.Failed ("BOM_8 not recognized");
   end if;
   if Encoding (BOM_16BE & "Hello") /= UTF_16BE or  -- A string with BOM
      Encoding (BOM_16BE) /= UTF_16BE               -- A string containing only BOM
   then
      Report.Failed ("BOM_16BE not recognized");
   end if;

   -- Check that conversions output BOM iff requested
   declare
      S1 : constant UTF_String := Convert ("Hello, Ada", Output_BOM => True);
      S2 : constant UTF_String := Convert ("Hello, Ada", Output_BOM => False);
   begin
      if S1 (1..3) /= BOM_8 then
	 Report.Failed ("Missing BOM at start of string");
      end if;
      if S2 /= S1 (4 .. S1'Last) then
	 Report.Failed ("Inconsistent value between BOM/no BOM");
      end if;
   end;

   -- Check that the lower bound is 1
   declare
      S : constant String (10..21) := "Elle souffle";
      U : UTF_8_String (10..24) := BOM_8 & To_String ("c3a0 c3a9 69 c3b4 c3bc e282ac"); -- "àéiôü€"
   begin
      if Encode (S, UTF_8)'First /= 1 then
	 Report.Failed ("Lower bound of result is not 1 (1)");
      end if;

      if Encode (S (12..15), UTF_16BE)'First /= 1 then
	 Report.Failed ("Lower bound of result is not 1 (2)");
      end if;

      if Convert (U, UTF_8, UTF_16LE)'First /= 1 then
	 Report.Failed ("Lower bound of result is not 1 (3)");
      end if;

      if Convert (U (15..19), UTF_8, UTF_16LE)'First /= 1 then
	 Report.Failed ("Lower bound of result is not 1 (4)");
      end if;
   exception
      when Occur : Encoding_Error =>
	 Report.Failed ("Encoding_Error in Convert (1)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
      when Occur : others =>
	 Report.Failed ("Other exception (1)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   -- Check detection of inconsistent BOM
   declare
      U : constant UTF_String := BOM_16BE & To_String ("0391 03a1 2c84");  -- Greek Alpha Rho Gamma
   begin
      if Convert (U, Input_Scheme => UTF_8, Output_Scheme => UTF_16LE) /= "" then
	 null;
      end if;
      Report.Failed ("Incorrect BOM did not raise Encoding_Error (1)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (2)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   -- Check detection (or not) of overlong encoding - Convert
   declare
      U : constant UTF_8_String := To_String ("F0 82 82 AC");  -- Overlong €
   begin
      if Convert (U, Input_Scheme => UTF_8, Output_Scheme => UTF_8) = To_String ("e282ac") then
	 Report.Comment ("Convert of overlong encoding accepted with correct result");
      elsif Convert (U, Input_Scheme => UTF_8, Output_Scheme => UTF_8) = U then
	 Report.Failed ("Convert of overlong encoding accepted with overlong result");
      else
	 Report.Failed ("Convert of overlong encoding accepted with incorrect result");
      end if;
   exception
      when Encoding_Error =>
	 Report.Failed ("Convert of overlong encoding raised Encoding_Error");
             -- AI12-0088-1 confirms that an exception is not to be raised here.
      when Occur : others =>
	 Report.Failed ("Other exception (3)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   -- Check detection (or not) of overlong encoding - Decode
   declare
      U : constant UTF_8_String := To_String ("C1 81");  -- Overlong Latin A
   begin
      if Decode (U, Input_Scheme => UTF_8) = "A" then
	 Report.Comment ("Decode of overlong encoding accepted with correct result");
      else
	 Report.Failed ("Decode of overlong encoding accepted with incorrect result");
      end if;
   exception
      when Encoding_Error =>
	 Report.Failed ("Decode of overlong encoding raised Encoding_Error");
             -- AI12-0088-1 confirms that an exception is not to be raised here.
      when Occur : others =>
	 Report.Failed ("Other exception (3)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   -- Check detection of invalid sequence
   declare
      U : constant UTF_String := To_String ("8080");  -- Invalid: first byte starts with 2#10xx xxxx#
   begin
      if Decode (U, Input_Scheme => UTF_8) /= "" then
	 null;
      end if;
      Report.Failed ("Invalid sequence did not raise Encoding_Error (1)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (4)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   declare
      U : constant UTF_String := To_String ("C0 C0");  -- Invalid: second byte starts with 2#11xx xxxx#
   begin
      if Decode (U, Input_Scheme => UTF_8) /= "" then
	 null;
      end if;
      Report.Failed ("Invalid sequence did not raise Encoding_Error (2)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (5)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   -- Check detection of UTF-16BE/LE strings with odd lengths
   declare
      U : constant UTF_String := BOM_16BE & To_String ("0391 03a1 2c");  -- Greek Alpha Rho and half Gamma
   begin
      if Decode (U, Input_Scheme => UTF_16BE) /= "" then
	 null;
      end if;
      Report.Failed ("Incorrect length did not raise Encoding_Error (1)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (6)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   declare
      U : constant UTF_String := BOM_16LE & To_String ("9103 a103 84");  -- Greek Alpha Rho and half Gamma
   begin
      if Decode (U, Input_Scheme => UTF_16LE) /= "" then
	 null;
      end if;
      Report.Failed ("Incorrect length did not raise Encoding_Error (1)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (7)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   -- Check detection of decoding to String a value above 16#FF#
   declare
      U : constant UTF_String := To_String ("CE91");  -- UTF_8 Greek Alpha
   begin
      if Decode (U) /= "" then
	 null;
      end if;
      Report.Failed ("Value above 16#ff# did not raise Encoding_Error (1)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (8)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   declare
      U : constant UTF_String := To_String ("0391");  -- UTF_16BE Greek Alpha
   begin
      if Decode (U, Input_Scheme => UTF_16BE) /= "" then
	 null;
      end if;
      Report.Failed ("Value above 16#ff# did not raise Encoding_Error (2)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (9)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   declare
      U : constant UTF_String := BOM_16LE & To_String ("9103");  -- UTF_16LE Greek Alpha
   begin
      if Decode (U, Input_Scheme => UTF_16LE) /= "" then
	 null;
      end if;
      Report.Failed ("Value above 16#ff# did not raise Encoding_Error (3)");
   exception
      when Encoding_Error =>
	 null;
      when Occur : others =>
	 Report.Failed ("Other exception (10)");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
   end;

   Report.Result;

exception
      when Occur : others =>
	 Report.Failed ("Exception at global level");
	 Report.Comment ("Exception message: " & Exception_Message (Occur));
	 Report.Result;
end CXA4036;


