------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . V M S _ E X C E P T I O N _ T A B L E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2009, Free Software Foundation, Inc.         --
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

--  This is an Alpha/VMS package

with System.HTable;
pragma Elaborate_All (System.HTable);

package body System.VMS_Exception_Table is

   use type SSL.Exception_Code;

   type HTable_Headers is range 1 .. 37;

   type Exception_Code_Data;
   type Exception_Code_Data_Ptr is access all Exception_Code_Data;

   --  The following record maps an imported VMS condition to an
   --  Ada exception.

   type Exception_Code_Data is record
      Code       : SSL.Exception_Code;
      Except     : SSL.Exception_Data_Ptr;
      HTable_Ptr : Exception_Code_Data_Ptr;
   end record;

   procedure Set_HT_Link
     (T    : Exception_Code_Data_Ptr;
      Next : Exception_Code_Data_Ptr);

   function Get_HT_Link (T : Exception_Code_Data_Ptr)
     return Exception_Code_Data_Ptr;

   function Hash (F : SSL.Exception_Code) return HTable_Headers;
   function Get_Key (T : Exception_Code_Data_Ptr) return SSL.Exception_Code;

   package Exception_Code_HTable is new System.HTable.Static_HTable (
     Header_Num => HTable_Headers,
     Element    => Exception_Code_Data,
     Elmt_Ptr   => Exception_Code_Data_Ptr,
     Null_Ptr   => null,
     Set_Next   => Set_HT_Link,
     Next       => Get_HT_Link,
     Key        => SSL.Exception_Code,
     Get_Key    => Get_Key,
     Hash       => Hash,
     Equal      => "=");

   ------------------
   -- Base_Code_In --
   ------------------

   function Base_Code_In
     (Code : SSL.Exception_Code) return SSL.Exception_Code
   is
   begin
      return Code and not 2#0111#;
   end Base_Code_In;

   ---------------------
   -- Coded_Exception --
   ---------------------

   function Coded_Exception
     (X : SSL.Exception_Code) return SSL.Exception_Data_Ptr
   is
      Res : Exception_Code_Data_Ptr;

   begin
      Res := Exception_Code_HTable.Get (X);

      if Res /= null  then
         return Res.Except;
      else
         return null;
      end if;

   end Coded_Exception;

   -----------------
   -- Get_HT_Link --
   -----------------

   function Get_HT_Link
     (T : Exception_Code_Data_Ptr) return Exception_Code_Data_Ptr
   is
   begin
      return T.HTable_Ptr;
   end Get_HT_Link;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (T : Exception_Code_Data_Ptr)
     return SSL.Exception_Code
   is
   begin
      return T.Code;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash
     (F : SSL.Exception_Code) return HTable_Headers
   is
      Headers_Magnitude : constant SSL.Exception_Code :=
        SSL.Exception_Code (HTable_Headers'Last - HTable_Headers'First + 1);

   begin
      return HTable_Headers (F mod Headers_Magnitude + 1);
   end Hash;

   ----------------------------
   -- Register_VMS_Exception --
   ----------------------------

   procedure Register_VMS_Exception
     (Code : SSL.Exception_Code;
      E    : SSL.Exception_Data_Ptr)
   is
      --  We bind the exception data with the base code found in the
      --  input value, that is with the severity bits masked off.

      Excode : constant SSL.Exception_Code := Base_Code_In (Code);

   begin
      --  The exception data registered here is mostly filled prior to this
      --  call and by __gnat_error_handler when the exception is raised. We
      --  still need to fill a couple of components for exceptions that will
      --  be used as propagation filters (exception data pointer registered
      --  as choices in the unwind tables): in some import/export cases, the
      --  exception pointers for the choice and the propagated occurrence may
      --  indeed be different for a single import code, and the personality
      --  routine attempts to match the import codes in this case.

      E.Lang := 'V';
      E.Import_Code := Excode;

      if Exception_Code_HTable.Get (Excode) = null then
         Exception_Code_HTable.Set (new Exception_Code_Data'(Excode, E, null));
      end if;
   end Register_VMS_Exception;

   -----------------
   -- Set_HT_Link --
   -----------------

   procedure Set_HT_Link
     (T    : Exception_Code_Data_Ptr;
      Next : Exception_Code_Data_Ptr)
   is
   begin
      T.HTable_Ptr := Next;
   end Set_HT_Link;

end System.VMS_Exception_Table;
