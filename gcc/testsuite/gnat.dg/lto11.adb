-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

with Ada.Streams; use Ada.Streams;

package body Lto11 is

   procedure Write
     (S : not null access Root_Stream_Type'Class;
      V : Vector)
   is
      subtype M_SEA is Stream_Element_Array (1 .. V'Size / Stream_Element'Size);
      Bytes : M_SEA;
      for Bytes'Address use V'Address;
      pragma Import (Ada, Bytes);
   begin
      Ada.Streams.Write (S.all, Bytes);
   end;

end Lto11;
