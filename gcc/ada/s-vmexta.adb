------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . V M S _ E X C E P T I O N _ T A B L E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1997-2001, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is an Alpha/VMS package.

with GNAT.HTable;
pragma Elaborate_All (GNAT.HTable);

package body System.VMS_Exception_Table is

   use System.Standard_Library;

   type HTable_Headers is range 1 .. 37;

   type Exception_Code_Data;
   type Exception_Code_Data_Ptr is access all Exception_Code_Data;

   --  The following record maps an imported VMS condition to an
   --  Ada exception.

   type Exception_Code_Data is record
      Code       : Natural;
      Except     : Exception_Data_Ptr;
      HTable_Ptr : Exception_Code_Data_Ptr;
   end record;

   procedure Set_HT_Link
     (T    : Exception_Code_Data_Ptr;
      Next : Exception_Code_Data_Ptr);

   function Get_HT_Link (T : Exception_Code_Data_Ptr)
     return Exception_Code_Data_Ptr;

   function Hash (F : Natural) return HTable_Headers;
   function Get_Key (T : Exception_Code_Data_Ptr) return Natural;

   package Exception_Code_HTable is new GNAT.HTable.Static_HTable (
     Header_Num => HTable_Headers,
     Element    => Exception_Code_Data,
     Elmt_Ptr   => Exception_Code_Data_Ptr,
     Null_Ptr   => null,
     Set_Next   => Set_HT_Link,
     Next       => Get_HT_Link,
     Key        => Natural,
     Get_Key    => Get_Key,
     Hash       => Hash,
     Equal      => "=");

   ---------------------
   -- Coded_Exception --
   ---------------------

   function Coded_Exception (X : Natural) return Exception_Data_Ptr is
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

   function  Get_HT_Link (T : Exception_Code_Data_Ptr)
     return Exception_Code_Data_Ptr is
   begin
      return T.HTable_Ptr;
   end Get_HT_Link;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (T : Exception_Code_Data_Ptr) return Natural is
   begin
      return T.Code;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (F : Natural) return HTable_Headers is
   begin
      return HTable_Headers
        (F mod Natural (HTable_Headers'Last - HTable_Headers'First + 1) + 1);
   end Hash;

   ----------------------------
   -- Register_VMS_Exception --
   ----------------------------

   procedure Register_VMS_Exception (Code : Integer) is
      --  Mask off lower 3 bits which are the severity

      Excode : Integer := (Code / 8) * 8;
   begin

      --  This allocates an empty exception that gets filled in by
      --  __gnat_error_handler when the exception is raised. Allocating
      --  it here prevents having to allocate it each time the exception
      --  is raised.

      if Exception_Code_HTable.Get (Excode) = null then
         Exception_Code_HTable.Set
           (new Exception_Code_Data'
             (Excode,
              new Exception_Data'(False, 'V', 0, null, null, 0),
              null));
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
