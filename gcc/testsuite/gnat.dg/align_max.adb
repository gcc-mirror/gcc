--  { dg-do run }

with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Deallocation;

procedure Align_MAX is

   Align : constant := Standard'Maximum_Alignment;

   generic
      type Data_Type (<>) is private;
      type Access_Type is access Data_Type;
      with function Allocate return Access_Type;
      with function Address (Ptr : Access_Type) return System.Address;
   package Check is
      --  The hooks below just force asm generation that helps associating
      --  obscure nested function names with their package instance name.
      Hook_Allocate : System.Address := Allocate'Address;
      Hook_Address : System.Address := Address'Address;
      pragma Volatile (Hook_Allocate);
      pragma Volatile (Hook_Address);

      procedure Run (Announce : String);
   end;

   package body Check is

      procedure Free is new
        Ada.Unchecked_Deallocation (Data_Type, Access_Type);

      procedure Run (Announce : String) is
         Addr : System.Address;
         Blocks : array (1 .. 1024) of Access_Type;
      begin
         for J in Blocks'Range loop
            Blocks (J) := Allocate;
            Addr := Address (Blocks (J));
            if Addr mod Data_Type'Alignment /= 0 then
               raise Program_Error;
            end if;
         end loop;

         for J in Blocks'Range loop
            Free (Blocks (J));
         end loop;
      end;
   end;

begin
   declare
      type Array_Type is array (Integer range <>) of Integer;
      for Array_Type'Alignment use Align;

      type FAT_Array_Access is access all Array_Type;

      function Allocate return FAT_Array_Access is
      begin
         return new Array_Type (1 .. 1);
      end;

      function Address (Ptr : FAT_Array_Access) return System.Address is
      begin
         return Ptr(1)'Address;
      end;
      package Check_FAT is new
        Check (Array_Type, FAT_Array_Access, Allocate, Address);
   begin
      Check_FAT.Run ("Checking FAT pointer to UNC array");
   end;

   declare
      type Array_Type is array (Integer range <>) of Integer;
      for Array_Type'Alignment use Align;

      type THIN_Array_Access is access all Array_Type;
      for THIN_Array_Access'Size use Standard'Address_Size;

      function Allocate return THIN_Array_Access is
      begin
         return new Array_Type (1 .. 1);
      end;

      function Address (Ptr : THIN_Array_Access) return System.Address is
      begin
         return Ptr(1)'Address;
      end;
      package Check_THIN is new
        Check (Array_Type, THIN_Array_Access, Allocate, Address);
   begin
      Check_THIN.Run ("Checking THIN pointer to UNC array");
   end;

   declare
      type Array_Type is array (Integer range 1 .. 1) of Integer;
      for Array_Type'Alignment use Align;

      type Array_Access is access all Array_Type;

      function Allocate return Array_Access is
      begin
         return new Array_Type;
      end;

      function Address (Ptr : Array_Access) return System.Address is
      begin
         return Ptr(1)'Address;
      end;
      package Check_Array is new
        Check (Array_Type, Array_Access, Allocate, Address);
   begin
      Check_Array.Run ("Checking pointer to constrained array");
   end;

   declare
      type Record_Type is record
         Value : Integer;
      end record;
      for Record_Type'Alignment use Align;

      type Record_Access is access all Record_Type;

      function Allocate return Record_Access is
      begin
         return new Record_Type;
      end;

      function Address (Ptr : Record_Access) return System.Address is
      begin
         return Ptr.all'Address;
      end;
      package Check_Record is new
        Check (Record_Type, Record_Access, Allocate, Address);
   begin
      Check_Record.Run ("Checking pointer to record");
   end;
end;

