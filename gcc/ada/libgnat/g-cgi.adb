------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T . C G I                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2001-2020, AdaCore                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Strings.Maps;

with GNAT.OS_Lib;
with GNAT.Table;

package body GNAT.CGI is

   use Ada;

   Valid_Environment : Boolean := True;
   --  This boolean will be set to False if the initialization was not
   --  completed correctly. It must be set to true there because the
   --  Initialize routine (called during elaboration) will use some of the
   --  services exported by this unit.

   Current_Method : Method_Type;
   --  This is the current method used to pass CGI parameters

   Header_Sent : Boolean := False;
   --  Will be set to True when the header will be sent

   --  Key/Value table declaration

   type String_Access is access String;

   type Key_Value is record
      Key   : String_Access;
      Value : String_Access;
   end record;

   package Key_Value_Table is new Table (Key_Value, Positive, 1, 1, 50);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Check_Environment;
   pragma Inline (Check_Environment);
   --  This procedure will raise Data_Error if Valid_Environment is False

   procedure Initialize;
   --  Initialize CGI package by reading the runtime environment. This
   --  procedure is called during elaboration. All exceptions raised during
   --  this procedure are deferred.

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      Check_Environment;
      return Key_Value_Table.Last;
   end Argument_Count;

   -----------------------
   -- Check_Environment --
   -----------------------

   procedure Check_Environment is
   begin
      if not Valid_Environment then
         raise Data_Error;
      end if;
   end Check_Environment;

   ------------
   -- Decode --
   ------------

   function Decode (S : String) return String is
      Result : String (S'Range);
      K      : Positive := S'First;
      J      : Positive := Result'First;

   begin
      while K <= S'Last loop
         if K + 2 <= S'Last
           and then S (K) = '%'
           and then Characters.Handling.Is_Hexadecimal_Digit (S (K + 1))
           and then Characters.Handling.Is_Hexadecimal_Digit (S (K + 2))
         then
            --  Here we have '%HH' which is an encoded character where 'HH' is
            --  the character number in hexadecimal.

            Result (J) := Character'Val
              (Natural'Value ("16#" & S (K + 1 .. K + 2) & '#'));
            K := K + 3;

         --  Plus sign is decoded as a space

         elsif S (K) = '+' then
            Result (J) := ' ';
            K := K + 1;

         else
            Result (J) := S (K);
            K := K + 1;
         end if;

         J := J + 1;
      end loop;

      return Result (Result'First .. J - 1);
   end Decode;

   -------------------------
   -- For_Every_Parameter --
   -------------------------

   procedure For_Every_Parameter is
      Quit : Boolean;

   begin
      Check_Environment;

      for K in 1 .. Key_Value_Table.Last loop

         Quit := False;

         Action (Key_Value_Table.Table (K).Key.all,
                 Key_Value_Table.Table (K).Value.all,
                 K,
                 Quit);

         exit when Quit;

      end loop;
   end For_Every_Parameter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      Request_Method : constant String :=
                         Characters.Handling.To_Upper
                           (Metavariable (CGI.Request_Method));

      procedure Initialize_GET;
      --  Read CGI parameters for a GET method. In this case the parameters
      --  are passed into QUERY_STRING environment variable.

      procedure Initialize_POST;
      --  Read CGI parameters for a POST method. In this case the parameters
      --  are passed with the standard input. The total number of characters
      --  for the data is passed in CONTENT_LENGTH environment variable.

      procedure Set_Parameter_Table (Data : String);
      --  Parse the parameter data and set the parameter table

      --------------------
      -- Initialize_GET --
      --------------------

      procedure Initialize_GET is
         Data : constant String := Metavariable (Query_String);
      begin
         Current_Method := Get;

         if Data /= "" then
            Set_Parameter_Table (Data);
         end if;
      end Initialize_GET;

      ---------------------
      -- Initialize_POST --
      ---------------------

      procedure Initialize_POST is
         Content_Length : constant Natural :=
                            Natural'Value (Metavariable (CGI.Content_Length));
         Data : String (1 .. Content_Length);

      begin
         Current_Method := Post;

         if Content_Length /= 0 then
            Text_IO.Get (Data);
            Set_Parameter_Table (Data);
         end if;
      end Initialize_POST;

      -------------------------
      -- Set_Parameter_Table --
      -------------------------

      procedure Set_Parameter_Table (Data : String) is

         procedure Add_Parameter (K : Positive; P : String);
         --  Add a single parameter into the table at index K. The parameter
         --  format is "key=value".

         Count : constant Positive :=
                   1 + Strings.Fixed.Count (Data, Strings.Maps.To_Set ("&"));
         --  Count is the number of parameters in the string. Parameters are
         --  separated by ampersand character.

         Index : Positive := Data'First;
         Amp   : Natural;

         -------------------
         -- Add_Parameter --
         -------------------

         procedure Add_Parameter (K : Positive; P : String) is
            Equal : constant Natural := Strings.Fixed.Index (P, "=");

         begin
            if Equal = 0 then
               raise Data_Error;

            else
               Key_Value_Table.Table (K) :=
                 Key_Value'(new String'(Decode (P (P'First .. Equal - 1))),
                            new String'(Decode (P (Equal + 1 .. P'Last))));
            end if;
         end Add_Parameter;

      --  Start of processing for Set_Parameter_Table

      begin
         Key_Value_Table.Set_Last (Count);

         for K in 1 .. Count - 1 loop
            Amp := Strings.Fixed.Index (Data (Index .. Data'Last), "&");

            Add_Parameter (K, Data (Index .. Amp - 1));

            Index := Amp + 1;
         end loop;

         --  add last parameter

         Add_Parameter (Count, Data (Index .. Data'Last));
      end Set_Parameter_Table;

   --  Start of processing for Initialize

   begin
      if Request_Method = "GET" then
         Initialize_GET;

      elsif Request_Method = "POST" then
         Initialize_POST;

      else
         Valid_Environment := False;
      end if;

   exception
      when others =>

         --  If we have an exception during initialization of this unit we
         --  just declare it invalid.

         Valid_Environment := False;
   end Initialize;

   ---------
   -- Key --
   ---------

   function Key (Position : Positive) return String is
   begin
      Check_Environment;

      if Position <= Key_Value_Table.Last then
         return Key_Value_Table.Table (Position).Key.all;
      else
         raise Parameter_Not_Found;
      end if;
   end Key;

   ----------------
   -- Key_Exists --
   ----------------

   function Key_Exists (Key : String) return Boolean is
   begin
      Check_Environment;

      for K in 1 .. Key_Value_Table.Last loop
         if Key_Value_Table.Table (K).Key.all = Key then
            return True;
         end if;
      end loop;

      return False;
   end Key_Exists;

   ------------------
   -- Metavariable --
   ------------------

   function Metavariable
     (Name     : Metavariable_Name;
      Required : Boolean := False) return String
   is
      function Get_Environment (Variable_Name : String) return String;
      --  Returns the environment variable content

      ---------------------
      -- Get_Environment --
      ---------------------

      function Get_Environment (Variable_Name : String) return String is
         Value  : OS_Lib.String_Access := OS_Lib.Getenv (Variable_Name);
         Result : constant String := Value.all;
      begin
         OS_Lib.Free (Value);
         return Result;
      end Get_Environment;

      Result : constant String :=
                 Get_Environment (Metavariable_Name'Image (Name));

   --  Start of processing for Metavariable

   begin
      Check_Environment;

      if Result = "" and then Required then
         raise Parameter_Not_Found;
      else
         return Result;
      end if;
   end Metavariable;

   -------------------------
   -- Metavariable_Exists --
   -------------------------

   function Metavariable_Exists (Name : Metavariable_Name) return Boolean is
   begin
      Check_Environment;

      if Metavariable (Name) = "" then
         return False;
      else
         return True;
      end if;
   end Metavariable_Exists;

   ------------
   -- Method --
   ------------

   function Method return Method_Type is
   begin
      Check_Environment;
      return Current_Method;
   end Method;

   --------
   -- Ok --
   --------

   function Ok return Boolean is
   begin
      return Valid_Environment;
   end Ok;

   ----------------
   -- Put_Header --
   ----------------

   procedure Put_Header
     (Header : String  := Default_Header;
      Force  : Boolean := False)
   is
   begin
      if Header_Sent = False or else Force then
         Check_Environment;
         Text_IO.Put_Line (Header);
         Text_IO.New_Line;
         Header_Sent := True;
      end if;
   end Put_Header;

   ---------
   -- URL --
   ---------

   function URL return String is

      function Exists_And_Not_80 (Server_Port : String) return String;
      --  Returns ':' & Server_Port if Server_Port is not "80" and the empty
      --  string otherwise (80 is the default sever port).

      -----------------------
      -- Exists_And_Not_80 --
      -----------------------

      function Exists_And_Not_80 (Server_Port : String) return String is
      begin
         if Server_Port = "80" then
            return "";
         else
            return ':' & Server_Port;
         end if;
      end Exists_And_Not_80;

   --  Start of processing for URL

   begin
      Check_Environment;

      return "http://"
        & Metavariable (Server_Name)
        & Exists_And_Not_80 (Metavariable (Server_Port))
        & Metavariable (Script_Name);
   end URL;

   -----------
   -- Value --
   -----------

   function Value
     (Key      : String;
      Required : Boolean := False)
      return     String
   is
   begin
      Check_Environment;

      for K in 1 .. Key_Value_Table.Last loop
         if Key_Value_Table.Table (K).Key.all = Key then
            return Key_Value_Table.Table (K).Value.all;
         end if;
      end loop;

      if Required then
         raise Parameter_Not_Found;
      else
         return "";
      end if;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Position : Positive) return String is
   begin
      Check_Environment;

      if Position <= Key_Value_Table.Last then
         return Key_Value_Table.Table (Position).Value.all;
      else
         raise Parameter_Not_Found;
      end if;
   end Value;

begin

   Initialize;

end GNAT.CGI;
