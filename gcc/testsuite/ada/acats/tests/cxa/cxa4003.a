-- CXA4003.A
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
--      Check that the subprograms defined in package Ada.Strings.Fixed are
--      available, and that they produce correct results.  Specifically, 
--      check the subprograms Index, Index_Non_Blank, Head, Tail, Translate, 
--      Find_Token, Move, Overwrite, and Replace_Slice.
--
-- TEST DESCRIPTION:
--      This test demonstrates how certain fixed string operations could be
--      used in string information processing.  A procedure is defined that
--      will extract portions of a 50 character string that correspond to
--      certain data items (i.e., name, address, state, zip code). These
--      parsed items will then be added to the appropriate fields of data
--      base elements.  These data base elements are then compared for
--      accuracy against a similar set of predefined data base elements.
--
--      A variety of fixed string processing subprograms are used in this
--      test.  Each parsing operation uses a different combination
--      of the available subprograms to accomplish the same goal, therefore
--      continuity of approach to string parsing is not seen in this test.
--      However, a wide variety of possible approaches are demonstrated, while
--      exercising a large number of the total predefined subprograms of
--      package Ada.Strings.Fixed.
--      
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Report;

procedure CXA4003 is

begin

   Report.Test ("CXA4003", "Check that the subprograms defined in package "  &
                           "Ada.Strings.Fixed are available, and that they " &
                           "produce correct results");

   Test_Block:
   declare

      Number_Of_Info_Strings : constant Natural := 3;
      DB_Size                : constant Natural := Number_Of_Info_Strings;
      Count                  : Natural          := 0;
      Finished_Processing    : Boolean          := False;
      Blank_String           : constant String  := " ";

      subtype Info_String_Type         is String (1..50);
      type    Info_String_Storage_Type is 
        array (1..Number_Of_Info_Strings) of Info_String_Type;


      subtype Name_Type                is String (1..10);
      subtype Street_Number_Type       is String (1..5);
      subtype Street_Name_Type         is String (1..10);
      subtype City_Type                is String (1..10);
      subtype State_Type               is String (1..2);
      subtype Zip_Code_Type            is String (1..5);

      type Data_Base_Element_Type is
         record
            Name          : Name_Type          := (others => ' ');
            Street_Number : Street_Number_Type := (others => ' ');
            Street_Name   : Street_Name_Type   := (others => ' ');
            City          : City_Type          := (others => ' ');
            State         : State_Type         := (others => ' ');
            Zip_Code      : Zip_Code_Type      := (others => ' ');
         end record;

      type Data_Base_Type is array (1..DB_Size) of Data_Base_Element_Type;

      Data_Base : Data_Base_Type;

      ---

      Info_String_1 : Info_String_Type := 
        "Joe_Jones 123   Sixth_St   San_Diego    CA   98765"; 

      Info_String_2 : Info_String_Type := 
        "Sam_Smith 56789  S._Seventh   Carlsbad  CA   92177";

      Info_String_3 : Info_String_Type := 
        "Jane_Brown 1219   Info_Lane  Tuscon     AZ   85643";


      Info_Strings : Info_String_Storage_Type := (1 => Info_String_1,
                                                  2 => Info_String_2,
                                                  3 => Info_String_3);



      TC_DB_Element_1 : Data_Base_Element_Type := 
        ("Joe Jones ", "123  ", "Sixth St  ", "San Diego ", "CA", "98765");

      TC_DB_Element_2 : Data_Base_Element_Type := 
        ("Sam Smith ", "56789", "S. Seventh", "Carlsbad  ", "CA", "92177");

      TC_DB_Element_3 : Data_Base_Element_Type := 
        ("Jane Brown", "1219 ", "Info Lane ", "Tuscon    ", "AZ", "85643");

      TC_Data_Base : Data_Base_Type := (TC_DB_Element_1,
                                        TC_DB_Element_2,
                                        TC_DB_Element_3);

      ---


      procedure Store_Information 
         (Info_String : in     Info_String_Type;
          DB_Record   : in out Data_Base_Element_Type) is

         package AS renames Ada.Strings;
         use type AS.Maps.Character_Set;

         UnderScore  : AS.Maps.Character_Sequence := "_";
         Blank       : AS.Maps.Character_Sequence := " ";

         Start,
         Stop  : Natural := 0;

         Underscore_to_Blank_Map : constant AS.Maps.Character_Mapping := 
                            AS.Maps.To_Mapping(From => UnderScore, 
                                               To   => Blank);

         Numeric_Set    : constant AS.Maps.Character_Set :=
                            AS.Maps.To_Set("0123456789");

         Cal            : constant AS.Maps.Character_Sequence := "CA";
         California_Set : constant AS.Maps.Character_Set := 
                            AS.Maps.To_Set(Cal);
         Arizona_Set    : constant AS.Maps.Character_Set := 
                            AS.Maps.To_Set("AZ");
         Nevada_Set     : constant AS.Maps.Character_Set := 
                            AS.Maps.To_Set("NV");

      begin

         -- Find the starting position of the name field (first non-blank),
         -- then, from that position, find the end of the name field (first
         -- blank).

         Start := AS.Fixed.Index_Non_Blank(Info_String); 
         Stop  := AS.Fixed.Index (Info_String(Start..Info_String'Length),
                                  AS.Maps.To_Set(' '),
                                  AS.Inside,
                                  AS.Forward) - 1 ;

         -- Store the name field in the data base element field for "Name".

         DB_Record.Name := AS.Fixed.Head(Info_String(1..Stop), 
                                         DB_Record.Name'Length);

         -- Replace any underscore characters in the name field
         -- that were used to separate first/middle/last names.

         AS.Fixed.Translate (DB_Record.Name, Underscore_to_Blank_Map);

         
         -- Continue the extraction process; now find the position of
         -- the street number in the string.

         Start := Stop + 1;

         AS.Fixed.Find_Token(Info_String(Start..Info_String'Length),
                             Numeric_Set,
                             AS.Inside,
                             Start,
                             Stop);

         -- Store the street number field in the appropriate data base 
         -- element.
         -- No modification of the default parameters of procedure Move
         -- is required.

         AS.Fixed.Move(Source => Info_String(Start..Stop),
                       Target => DB_Record.Street_Number);


         -- Continue the extraction process; find the street name in the
         -- info string.  Skip blanks to the start of the street name, then
         -- search for the index of the next blank character in the string.

         Start :=
           AS.Fixed.Index_Non_Blank(Info_String(Stop+1..Info_String'Length)); 

         Stop := 
           AS.Fixed.Index(Info_String(Start..Info_String'Length),
                          Blank_String) - 1; 

         -- Store the street name in the appropriate data base element field.

         AS.Fixed.Overwrite(DB_Record.Street_Name, 
                            1, 
                            Info_String(Start..Stop));
                           
         -- Replace any underscore characters in the street name field
         -- that were used as word separation.

         DB_Record.Street_Name := AS.Fixed.Translate(DB_Record.Street_Name, 
                                                     Underscore_to_Blank_Map);


         -- Continue the extraction; remove the city name from the string.

         Start :=
           AS.Fixed.Index_Non_Blank(Info_String(Stop+1..Info_String'Length)); 

         Stop := 
           AS.Fixed.Index(Info_String(Start..Info_String'Length), 
                          Blank_String) - 1; 

         -- Store the city name field in the appropriate data base element.

         AS.Fixed.Replace_Slice(DB_Record.City, 
                                1,
                                DB_Record.City'Length,
                                Info_String(Start..Stop));
                           
         -- Replace any underscore characters in the city name field
         -- that were used as word separation.

         AS.Fixed.Translate (DB_Record.City, Underscore_to_Blank_Map);


         -- Continue the extraction; remove the state identifier from the 
         -- info string.

         Start := Stop + 1;

         AS.Fixed.Find_Token(Info_String(Start..Info_String'Length),
                             AS.Maps."OR"(California_Set,
                               AS.Maps."OR"(Nevada_Set, Arizona_Set)),
                             AS.Inside,
                             Start,
                             Stop);

         -- Store the state indicator into the data base element.

         AS.Fixed.Move(Source  => Info_String(Start..Stop),
                       Target  => DB_Record.State,
                       Drop    => Ada.Strings.Right,
                       Justify => Ada.Strings.Left,
                       Pad     => AS.Space);


         -- Continue the extraction process; remove the final data item in 
         -- the info string, the zip code, and place it into the 
         -- corresponding data base element.

         DB_Record.Zip_Code := AS.Fixed.Tail(Info_String, 
                                             DB_Record.Zip_Code'Length);

      exception 
         when AS.Length_Error      =>
            Report.Failed ("Length_Error raised in procedure");
         when AS.Pattern_Error     =>
            Report.Failed ("Pattern_Error raised in procedure");
         when AS.Translation_Error =>
            Report.Failed ("Translation_Error raised in procedure");
         when others            =>
            Report.Failed ("Exception raised in procedure");
      end Store_Information;


   begin

      -- Loop thru the information strings, extract the name and address
      -- information, place this info into elements of the data base.

      while not Finished_Processing loop

         Count := Count + 1;

         Store_Information (Info_Strings(Count), Data_Base(Count));

         Finished_Processing := (Count = Number_Of_Info_Strings);

      end loop;


      -- Verify that the string processing was successful.

      for i in 1..DB_Size loop
         if Data_Base(i) /= TC_Data_Base(i) then
            Report.Failed 
              ("Data processing error on record " & Integer'Image(i));
         end if;
      end loop;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;


   Report.Result;

end CXA4003;
