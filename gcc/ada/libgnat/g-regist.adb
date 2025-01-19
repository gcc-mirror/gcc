------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T . R E G I S T R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2001-2025, Free Software Foundation, Inc.        --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with System;
with GNAT.Directory_Operations;

package body GNAT.Registry is

   use System;

   ------------------------------
   -- Binding to the Win32 API --
   ------------------------------

   subtype LONG is Interfaces.C.long;
   subtype ULONG is Interfaces.C.unsigned_long;
   subtype DWORD is ULONG;

   type    PULONG is access all ULONG;
   subtype PDWORD is PULONG;
   subtype LPDWORD is PDWORD;

   subtype Error_Code is LONG;

   subtype REGSAM is LONG;

   type PHKEY is access all HKEY;

   ERROR_SUCCESS : constant Error_Code := 0;

   REG_SZ        : constant := 1;
   REG_EXPAND_SZ : constant := 2;

   function RegCloseKey (Key : HKEY) return LONG;
   pragma Import (Stdcall, RegCloseKey, "RegCloseKey");

   function RegCreateKeyEx
     (Key                  : HKEY;
      lpSubKey             : Address;
      Reserved             : DWORD;
      lpClass              : Address;
      dwOptions            : DWORD;
      samDesired           : REGSAM;
      lpSecurityAttributes : Address;
      phkResult            : PHKEY;
      lpdwDisposition      : LPDWORD)
      return                 LONG;
   pragma Import (Stdcall, RegCreateKeyEx, "RegCreateKeyExA");

   function RegDeleteKey
     (Key      : HKEY;
      lpSubKey : Address) return LONG;
   pragma Import (Stdcall, RegDeleteKey, "RegDeleteKeyA");

   function RegDeleteValue
     (Key         : HKEY;
      lpValueName : Address) return LONG;
   pragma Import (Stdcall, RegDeleteValue, "RegDeleteValueA");

   function RegEnumValue
     (Key           : HKEY;
      dwIndex       : DWORD;
      lpValueName   : Address;
      lpcbValueName : LPDWORD;
      lpReserved    : LPDWORD;
      lpType        : LPDWORD;
      lpData        : Address;
      lpcbData      : LPDWORD) return LONG;
   pragma Import (Stdcall, RegEnumValue, "RegEnumValueA");

   function RegOpenKeyEx
     (Key        : HKEY;
      lpSubKey   : Address;
      ulOptions  : DWORD;
      samDesired : REGSAM;
      phkResult  : PHKEY) return LONG;
   pragma Import (Stdcall, RegOpenKeyEx, "RegOpenKeyExA");

   function RegQueryValueEx
     (Key         : HKEY;
      lpValueName : Address;
      lpReserved  : LPDWORD;
      lpType      : LPDWORD;
      lpData      : Address;
      lpcbData    : LPDWORD) return LONG;
   pragma Import (Stdcall, RegQueryValueEx, "RegQueryValueExA");

   function RegSetValueEx
     (Key         : HKEY;
      lpValueName : Address;
      Reserved    : DWORD;
      dwType      : DWORD;
      lpData      : Address;
      cbData      : DWORD) return LONG;
   pragma Import (Stdcall, RegSetValueEx, "RegSetValueExA");

   function RegEnumKey
     (Key         : HKEY;
      dwIndex     : DWORD;
      lpName      : Address;
      cchName     : DWORD) return LONG;
   pragma Import (Stdcall, RegEnumKey, "RegEnumKeyA");

   ---------------------
   -- Local Constants --
   ---------------------

   Max_Key_Size : constant := 1_024;
   --  Maximum number of characters for a registry key

   Max_Value_Size : constant := 2_048;
   --  Maximum number of characters for a key's value

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_C_Mode (Mode : Key_Mode) return REGSAM;
   --  Returns the Win32 mode value for the Key_Mode value

   procedure Check_Result (Result : LONG; Message : String);
   --  Checks value Result and raise the exception Registry_Error if it is not
   --  equal to ERROR_SUCCESS. Message and the error value (Result) is added
   --  to the exception message.

   ------------------
   -- Check_Result --
   ------------------

   procedure Check_Result (Result : LONG; Message : String) is
      use type LONG;
   begin
      if Result /= ERROR_SUCCESS then
         raise Registry_Error with
           Message & " (" & LONG'Image (Result) & ')';
      end if;
   end Check_Result;

   ---------------
   -- Close_Key --
   ---------------

   procedure Close_Key (Key : HKEY) is
      Result : LONG;
   begin
      Result := RegCloseKey (Key);
      Check_Result (Result, "Close_Key");
   end Close_Key;

   ----------------
   -- Create_Key --
   ----------------

   function Create_Key
     (From_Key : HKEY;
      Sub_Key  : String;
      Mode     : Key_Mode := Read_Write) return HKEY
   is
      REG_OPTION_NON_VOLATILE : constant := 16#0#;

      C_Sub_Key : constant String := Sub_Key & ASCII.NUL;
      C_Class   : constant String := "" & ASCII.NUL;
      C_Mode    : constant REGSAM := To_C_Mode (Mode);

      New_Key : aliased HKEY;
      Result  : LONG;
      Dispos  : aliased DWORD;

   begin
      Result :=
        RegCreateKeyEx
          (From_Key,
           C_Sub_Key (C_Sub_Key'First)'Address,
           0,
           C_Class (C_Class'First)'Address,
           REG_OPTION_NON_VOLATILE,
           C_Mode,
           Null_Address,
           New_Key'Unchecked_Access,
           Dispos'Unchecked_Access);

      Check_Result (Result, "Create_Key " & Sub_Key);
      return New_Key;
   end Create_Key;

   ----------------
   -- Delete_Key --
   ----------------

   procedure Delete_Key (From_Key : HKEY; Sub_Key : String) is
      C_Sub_Key : constant String := Sub_Key & ASCII.NUL;
      Result    : LONG;
   begin
      Result := RegDeleteKey (From_Key, C_Sub_Key (C_Sub_Key'First)'Address);
      Check_Result (Result, "Delete_Key " & Sub_Key);
   end Delete_Key;

   ------------------
   -- Delete_Value --
   ------------------

   procedure Delete_Value (From_Key : HKEY; Sub_Key : String) is
      C_Sub_Key : constant String := Sub_Key & ASCII.NUL;
      Result    : LONG;
   begin
      Result := RegDeleteValue (From_Key, C_Sub_Key (C_Sub_Key'First)'Address);
      Check_Result (Result, "Delete_Value " & Sub_Key);
   end Delete_Value;

   -------------------
   -- For_Every_Key --
   -------------------

   procedure For_Every_Key
     (From_Key  : HKEY;
      Recursive : Boolean := False)
   is
      procedure Recursive_For_Every_Key
        (From_Key  : HKEY;
         Recursive : Boolean := False;
         Quit      : in out Boolean);

      -----------------------------
      -- Recursive_For_Every_Key --
      -----------------------------

      procedure Recursive_For_Every_Key
        (From_Key : HKEY;
         Recursive : Boolean := False;
         Quit      : in out Boolean)
      is
         use type LONG;
         use type ULONG;

         Index  : ULONG := 0;
         Result : LONG;

         Sub_Key : Interfaces.C.char_array (1 .. Max_Key_Size);
         pragma Warnings (Off, Sub_Key);

         Size_Sub_Key : aliased ULONG;
         Sub_Hkey     : HKEY;

         function Current_Name return String;

         ------------------
         -- Current_Name --
         ------------------

         function Current_Name return String is
         begin
            return Interfaces.C.To_Ada (Sub_Key);
         end Current_Name;

      --  Start of processing for Recursive_For_Every_Key

      begin
         loop
            Size_Sub_Key := Sub_Key'Length;

            Result :=
              RegEnumKey
                (From_Key, Index, Sub_Key (1)'Address, Size_Sub_Key);

            exit when not (Result = ERROR_SUCCESS);

            Sub_Hkey := Open_Key (From_Key, Interfaces.C.To_Ada (Sub_Key));

            Action (Natural (Index) + 1, Sub_Hkey, Current_Name, Quit);

            if not Quit and then Recursive then
               Recursive_For_Every_Key (Sub_Hkey, True, Quit);
            end if;

            Close_Key (Sub_Hkey);

            exit when Quit;

            Index := Index + 1;
         end loop;
      end Recursive_For_Every_Key;

      --  Local Variables

      Quit : Boolean := False;

   --  Start of processing for For_Every_Key

   begin
      Recursive_For_Every_Key (From_Key, Recursive, Quit);
   end For_Every_Key;

   -------------------------
   -- For_Every_Key_Value --
   -------------------------

   procedure For_Every_Key_Value
     (From_Key : HKEY;
      Expand   : Boolean := False)
   is
      use GNAT.Directory_Operations;
      use type LONG;
      use type ULONG;

      Index  : ULONG := 0;
      Result : LONG;

      Sub_Key : String (1 .. Max_Key_Size);
      pragma Warnings (Off, Sub_Key);

      Value : String (1 .. Max_Value_Size);
      pragma Warnings (Off, Value);

      Size_Sub_Key : aliased ULONG;
      Size_Value   : aliased ULONG;
      Type_Sub_Key : aliased DWORD;

      Quit : Boolean;

   begin
      loop
         Size_Sub_Key := Sub_Key'Length;
         Size_Value   := Value'Length;

         Result :=
           RegEnumValue
             (From_Key, Index,
              Sub_Key (1)'Address,
              Size_Sub_Key'Unchecked_Access,
              null,
              Type_Sub_Key'Unchecked_Access,
              Value (1)'Address,
              Size_Value'Unchecked_Access);

         exit when not (Result = ERROR_SUCCESS);

         Quit := False;

         if Type_Sub_Key = REG_EXPAND_SZ and then Expand then
            Action
              (Natural (Index) + 1,
               Sub_Key (1 .. Integer (Size_Sub_Key)),
               Directory_Operations.Expand_Path
                 (Value (1 .. Integer (Size_Value) - 1),
                  Directory_Operations.DOS),
               Quit);

         elsif Type_Sub_Key = REG_SZ or else Type_Sub_Key = REG_EXPAND_SZ then
            Action
              (Natural (Index) + 1,
               Sub_Key (1 .. Integer (Size_Sub_Key)),
               Value (1 .. Integer (Size_Value) - 1),
               Quit);
         end if;

         exit when Quit;

         Index := Index + 1;
      end loop;
   end For_Every_Key_Value;

   ----------------
   -- Key_Exists --
   ----------------

   function Key_Exists
     (From_Key : HKEY;
      Sub_Key  : String) return Boolean
   is
      New_Key : HKEY;

   begin
      New_Key := Open_Key (From_Key, Sub_Key);
      Close_Key (New_Key);

      --  We have been able to open the key so it exists

      return True;

   exception
      when Registry_Error =>

         --  An error occurred, the key was not found

         return False;
   end Key_Exists;

   --------------
   -- Open_Key --
   --------------

   function Open_Key
     (From_Key : HKEY;
      Sub_Key  : String;
      Mode     : Key_Mode := Read_Only) return HKEY
   is
      C_Sub_Key : constant String := Sub_Key & ASCII.NUL;
      C_Mode    : constant REGSAM := To_C_Mode (Mode);

      New_Key : aliased HKEY;
      Result  : LONG;

   begin
      Result :=
        RegOpenKeyEx
          (From_Key,
           C_Sub_Key (C_Sub_Key'First)'Address,
           0,
           C_Mode,
           New_Key'Unchecked_Access);

      Check_Result (Result, "Open_Key " & Sub_Key);
      return New_Key;
   end Open_Key;

   -----------------
   -- Query_Value --
   -----------------

   function Query_Value
     (From_Key : HKEY;
      Sub_Key  : String;
      Expand   : Boolean := False) return String
   is
      use GNAT.Directory_Operations;
      use type ULONG;

      Value : String (1 .. Max_Value_Size);
      pragma Warnings (Off, Value);

      Size_Value : aliased ULONG;
      Type_Value : aliased DWORD;

      C_Sub_Key : constant String := Sub_Key & ASCII.NUL;
      Result    : LONG;

   begin
      Size_Value := Value'Length;

      Result :=
        RegQueryValueEx
          (From_Key,
           C_Sub_Key (C_Sub_Key'First)'Address,
           null,
           Type_Value'Unchecked_Access,
           Value (Value'First)'Address,
           Size_Value'Unchecked_Access);

      Check_Result (Result, "Query_Value " & Sub_Key & " key");

      if Type_Value = REG_EXPAND_SZ and then Expand then
         return Directory_Operations.Expand_Path
           (Value (1 .. Integer (Size_Value - 1)),
            Directory_Operations.DOS);
      else
         return Value (1 .. Integer (Size_Value - 1));
      end if;
   end Query_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (From_Key : HKEY;
       Sub_Key  : String;
       Value    : String;
       Expand   : Boolean := False)
   is
      C_Sub_Key : constant String := Sub_Key & ASCII.NUL;
      C_Value   : constant String := Value & ASCII.NUL;

      Value_Type : DWORD;
      Result     : LONG;

   begin
      Value_Type := (if Expand then REG_EXPAND_SZ else REG_SZ);

      Result :=
        RegSetValueEx
          (From_Key,
           C_Sub_Key (C_Sub_Key'First)'Address,
           0,
           Value_Type,
           C_Value (C_Value'First)'Address,
           C_Value'Length);

      Check_Result (Result, "Set_Value " & Sub_Key & " key");
   end Set_Value;

   ---------------
   -- To_C_Mode --
   ---------------

   function To_C_Mode (Mode : Key_Mode) return REGSAM is
      use type REGSAM;

      KEY_READ        : constant := 16#20019#;
      KEY_WRITE       : constant := 16#20006#;
      KEY_WOW64_64KEY : constant := 16#00100#;
      KEY_WOW64_32KEY : constant := 16#00200#;

   begin
      case Mode is
         when Read_Only =>
            return KEY_READ + KEY_WOW64_32KEY;

         when Read_Write =>
            return KEY_READ + KEY_WRITE + KEY_WOW64_32KEY;

         when Read_Only_64 =>
            return KEY_READ + KEY_WOW64_64KEY;

         when Read_Write_64 =>
            return KEY_READ + KEY_WRITE + KEY_WOW64_64KEY;
      end case;
   end To_C_Mode;

end GNAT.Registry;
