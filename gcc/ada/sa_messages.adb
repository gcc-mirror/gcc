------------------------------------------------------------------------------
--                       C O D E P E E R / S P A R K                        --
--                                                                          --
--                     Copyright (C) 2015-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded.Hash;

with Ada.Text_IO;     use Ada.Text_IO;
with GNATCOLL.JSON;   use GNATCOLL.JSON;

package body SA_Messages is

   -----------------------
   -- Local subprograms --
   -----------------------

   function "<" (Left, Right : SA_Message) return Boolean is
     (if Left.Kind /= Right.Kind then
         Left.Kind < Right.Kind
      else
         Left.Kind in Check_Kind
           and then Left.Check_Result < Right.Check_Result);

   function "<" (Left, Right : Simple_Source_Location) return Boolean is
      (if Left.File_Name /= Right.File_Name then
          Left.File_Name < Right.File_Name
       elsif Left.Line /= Right.Line then
          Left.Line < Right.Line
       else
          Left.Column < Right.Column);

   function "<" (Left, Right : Source_Locations) return Boolean is
     (if Left'Length /= Right'Length then
         Left'Length < Right'Length
      elsif Left'Length = 0 then
         False
      elsif Left (Left'Last) /= Right (Right'Last) then
         Left (Left'Last) < Right (Right'Last)
      else
         Left (Left'First .. Left'Last - 1) <
           Right (Right'First .. Right'Last - 1));

   function "<" (Left, Right : Source_Location) return Boolean is
     (Left.Locations < Right.Locations);

   function Base_Location
     (Location : Source_Location) return Simple_Source_Location is
     (Location.Locations (1));

   function Hash (Key : SA_Message) return Hash_Type;
   function Hash (Key : Source_Location) return Hash_Type;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Message_And_Location) return Boolean is
     (if Left.Message = Right.Message
      then Left.Location < Right.Location
      else Left.Message < Right.Message);

   ------------
   -- Column --
   ------------

   function Column (Location : Source_Location) return Column_Number is
     (Base_Location (Location).Column);

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Location : Source_Location) return String is
     (To_String (Base_Location (Location).File_Name));

   function File_Name (Location : Source_Location) return Unbounded_String is
     (Base_Location (Location).File_Name);

   ------------------------
   -- Enclosing_Instance --
   ------------------------

   function Enclosing_Instance
     (Location : Source_Location) return Source_Location_Or_Null is
     (Count     => Location.Count - 1,
      Locations => Location.Locations (2 .. Location.Count));

   ----------
   -- Hash --
   ----------

   function Hash (Key : Message_And_Location) return Hash_Type is
     (Hash (Key.Message) + Hash (Key.Location));

   function Hash (Key : SA_Message) return Hash_Type is
   begin
      return Result : Hash_Type :=
                        Hash_Type'Mod (Message_Kind'Pos (Key.Kind))
      do
         if Key.Kind in Check_Kind then
            Result := Result +
              Hash_Type'Mod (SA_Check_Result'Pos (Key.Check_Result));
         end if;
      end return;
   end Hash;

   function Hash (Key : Source_Location) return Hash_Type is
   begin
      return Result : Hash_Type := Hash_Type'Mod (Key.Count) do
         for Loc of Key.Locations loop
            Result := Result + Hash (Loc.File_Name);
            Result := Result + Hash_Type'Mod (Loc.Line);
            Result := Result + Hash_Type'Mod (Loc.Column);
         end loop;
      end return;
   end Hash;

   ---------------
   -- Iteration --
   ---------------

   function Iteration (Location : Source_Location) return Iteration_Id is
     (Base_Location (Location).Iteration);

   ----------
   -- Line --
   ----------

   function Line (Location : Source_Location) return Line_Number is
     (Base_Location (Location).Line);

   --------------
   -- Location --
   --------------

   function Location
     (Item : Message_And_Location) return Source_Location is
     (Item.Location);

   ----------
   -- Make --
   ----------

   function Make
     (File_Name          : String;
      Line               : Line_Number;
      Column             : Column_Number;
      Iteration          : Iteration_Id;
      Enclosing_Instance : Source_Location_Or_Null) return Source_Location
   is
   begin
      return Result : Source_Location
                        (Count => Enclosing_Instance.Count + 1)
      do
         Result.Locations (1) :=
           (File_Name => To_Unbounded_String (File_Name),
            Line      => Line,
            Column    => Column,
            Iteration => Iteration);

         Result.Locations (2 .. Result.Count) := Enclosing_Instance.Locations;
      end return;
   end Make;

   ------------------
   -- Make_Msg_Loc --
   ------------------

   function Make_Msg_Loc
     (Msg : SA_Message;
      Loc : Source_Location) return Message_And_Location
   is
   begin
      return Message_And_Location'(Count    => Loc.Count,
                                   Message  => Msg,
                                   Location => Loc);
   end Make_Msg_Loc;

   -------------
   -- Message --
   -------------

   function Message (Item : Message_And_Location) return SA_Message is
     (Item.Message);

   package Field_Names is

      --  A Source_Location value is represented in JSON as a two or three
      --  field value having fields Message_Kind (a string) and Locations (an
      --  array); if the Message_Kind indicates a check kind, then a third
      --  field is present: Check_Result (a string). The element type of the
      --  Locations array is a value having at least 4 fields:
      --  File_Name (a string), Line (an integer), Column (an integer),
      --  and Iteration_Kind (an integer); if the Iteration_Kind field
      --  has the value corresponding to the enumeration literal Numbered,
      --  then two additional integer fields are present, Iteration_Number
      --  and Iteration_Of_Total.

      Check_Result       : constant String := "Check_Result";
      Column             : constant String := "Column";
      File_Name          : constant String := "File_Name";
      Iteration_Kind     : constant String := "Iteration_Kind";
      Iteration_Number   : constant String := "Iteration_Number";
      Iteration_Of_Total : constant String := "Iteration_Total";
      Line               : constant String := "Line";
      Locations          : constant String := "Locations";
      Message_Kind       : constant String := "Message_Kind";
      Messages           : constant String := "Messages";
   end Field_Names;

   package body Writing is
      File : File_Type;
      --  The file to which output will be written (in Close, not in Write)

      Messages : JSON_Array;
      --  Successive calls to Write append messages to this list

      -----------------------
      -- Local subprograms --
      -----------------------

      function To_JSON_Array
        (Locations : Source_Locations) return JSON_Array;
      --  Represent a Source_Locations array as a JSON_Array

      function To_JSON_Value
        (Location : Simple_Source_Location) return JSON_Value;
      --  Represent a Simple_Source_Location as a JSON_Value

      -----------
      -- Close --
      -----------

      procedure Close is
         Value : constant JSON_Value := Create_Object;

      begin
         --  only one field for now
         Set_Field (Value, Field_Names.Messages, Messages);
         Put_Line (File, Write (Item => Value, Compact => False));
         Clear (Messages);
         Close (File => File);
      end Close;

      -------------
      -- Is_Open --
      -------------

      function Is_Open return Boolean is (Is_Open (File));

      ----------
      -- Open --
      ----------

      procedure Open (File_Name : String) is
      begin
         Create (File => File, Mode => Out_File, Name => File_Name);
         Clear (Messages);
      end Open;

      -------------------
      -- To_JSON_Array --
      -------------------

      function To_JSON_Array
        (Locations : Source_Locations) return JSON_Array
      is
      begin
         return Result : JSON_Array := Empty_Array do
            for Location of Locations loop
               Append (Result, To_JSON_Value (Location));
            end loop;
         end return;
      end To_JSON_Array;

      -------------------
      -- To_JSON_Value --
      -------------------

      function To_JSON_Value
        (Location : Simple_Source_Location) return JSON_Value
      is
      begin
         return Result : constant JSON_Value := Create_Object do
            Set_Field (Result, Field_Names.File_Name, Location.File_Name);
            Set_Field (Result, Field_Names.Line, Integer (Location.Line));
            Set_Field (Result, Field_Names.Column, Integer (Location.Column));
            Set_Field (Result, Field_Names.Iteration_Kind, Integer'(
                       Iteration_Kind'Pos (Location.Iteration.Kind)));

            if Location.Iteration.Kind = Numbered then
               Set_Field (Result, Field_Names.Iteration_Number,
                          Location.Iteration.Number);
               Set_Field (Result, Field_Names.Iteration_Of_Total,
                          Location.Iteration.Of_Total);
            end if;
         end return;
      end To_JSON_Value;

      -----------
      -- Write --
      -----------

      procedure Write (Message : SA_Message; Location : Source_Location) is
         Value : constant JSON_Value := Create_Object;

      begin
         Set_Field (Value, Field_Names.Message_Kind, Message.Kind'Img);

         if Message.Kind in Check_Kind then
            Set_Field
              (Value, Field_Names.Check_Result, Message.Check_Result'Img);
         end if;

         Set_Field
           (Value, Field_Names.Locations, To_JSON_Array (Location.Locations));
         Append (Messages, Value);
      end Write;
   end Writing;

   package body Reading is
      File       : File_Type;
      --  The file from which messages are read (in Open, not in Read)

      Messages   : JSON_Array;
      --  The list of messages that were read in from File

      Next_Index : Positive;
      --  The index of the message in Messages which will be returned by the
      --  next call to Get.

      Parse_Full_Path : Boolean := True;
      --  if the full path or only the base name of the file should be parsed

      -----------
      -- Close --
      -----------

      procedure Close is
      begin
         Clear (Messages);
         Close (File);
      end Close;

      ----------
      -- Done --
      ----------

      function Done return Boolean is (Next_Index > Length (Messages));

      ---------
      -- Get --
      ---------

      function Get return Message_And_Location is
         Value : constant JSON_Value := Get (Messages, Next_Index);

         function Get_Message (Kind :  Message_Kind) return SA_Message;
         --  Return SA_Message of given kind, filling in any non-discriminant
         --  by reading from Value.

         function Make
           (Location : Source_Location;
            Message  : SA_Message) return Message_And_Location;
         --  Constructor

         function To_Location
           (Encoded   : JSON_Array;
            Full_Path : Boolean) return Source_Location;
         --  Decode a Source_Location from JSON_Array representation

         function To_Simple_Location
           (Encoded   : JSON_Value;
            Full_Path : Boolean) return Simple_Source_Location;
         --  Decode a Simple_Source_Location from JSON_Value representation

         -----------------
         -- Get_Message --
         -----------------

         function Get_Message (Kind :  Message_Kind) return SA_Message is
         begin
            --  If we had AI12-0086, then we could use aggregates here (which
            --  would be better than field-by-field assignment for the usual
            --  maintainability reasons). But we don't, so we won't.

            return Result : SA_Message (Kind => Kind) do
               if Kind in Check_Kind then
                  Result.Check_Result :=
                    SA_Check_Result'Value
                      (Get (Value, Field_Names.Check_Result));
               end if;
            end return;
         end Get_Message;

         ----------
         -- Make --
         ----------

         function Make
           (Location : Source_Location;
            Message  : SA_Message) return Message_And_Location
         is
           (Count => Location.Count, Message => Message, Location => Location);

         -----------------
         -- To_Location --
         -----------------

         function To_Location
           (Encoded   : JSON_Array;
            Full_Path : Boolean) return Source_Location is
         begin
            return Result : Source_Location (Count => Length (Encoded)) do
               for I in Result.Locations'Range loop
                  Result.Locations (I) :=
                    To_Simple_Location (Get (Encoded, I), Full_Path);
               end loop;
            end return;
         end To_Location;

         ------------------------
         -- To_Simple_Location --
         ------------------------

         function To_Simple_Location
           (Encoded   : JSON_Value;
            Full_Path : Boolean) return Simple_Source_Location
         is
            function Get_Iteration_Id
              (Kind : Iteration_Kind) return Iteration_Id;
            --  Given the discriminant for an Iteration_Id value, return the
            --  entire value.

            ----------------------
            -- Get_Iteration_Id --
            ----------------------

            function Get_Iteration_Id (Kind : Iteration_Kind)
              return Iteration_Id
            is
            begin
               --  Initialize non-discriminant fields, if any

               return Result : Iteration_Id (Kind => Kind) do
                  if Kind = Numbered then
                     Result :=
                       (Kind     => Numbered,
                        Number   =>
                          Get (Encoded, Field_Names.Iteration_Number),
                        Of_Total =>
                          Get (Encoded, Field_Names.Iteration_Of_Total));
                  end if;
               end return;
            end Get_Iteration_Id;

            --  Local variables

            FN : constant Unbounded_String :=
                   Get (Encoded, Field_Names.File_Name);

         --  Start of processing for To_Simple_Location

         begin
            return
              (File_Name =>
                 (if Full_Path then
                     FN
                  else
                     To_Unbounded_String (Simple_Name (To_String (FN)))),
               Line      =>
                 Line_Number (Integer'(Get (Encoded, Field_Names.Line))),
               Column    =>
                 Column_Number (Integer'(Get (Encoded, Field_Names.Column))),
               Iteration =>
                 Get_Iteration_Id
                   (Kind => Iteration_Kind'Val (Integer'(Get
                              (Encoded, Field_Names.Iteration_Kind)))));
         end To_Simple_Location;

      --  Start of processing for Get

      begin
         Next_Index := Next_Index + 1;

         return Make
           (Message  =>
              Get_Message
                (Message_Kind'Value (Get (Value, Field_Names.Message_Kind))),
            Location =>
              To_Location
                (Get (Value, Field_Names.Locations), Parse_Full_Path));
      end Get;

      -------------
      -- Is_Open --
      -------------

      function Is_Open return Boolean is (Is_Open (File));

      ----------
      -- Open --
      ----------

      procedure Open (File_Name : String; Full_Path : Boolean := True) is
         File_Text : Unbounded_String := Null_Unbounded_String;

      begin
         Parse_Full_Path := Full_Path;
         Open (File => File, Mode => In_File, Name => File_Name);

         --  File read here, not in Get, but that's an implementation detail

         while not End_Of_File (File) loop
            Append (File_Text, Get_Line (File));
         end loop;

         Messages   := Get (Read (File_Text), Field_Names.Messages);
         Next_Index := 1;
      end Open;
   end Reading;

end SA_Messages;
