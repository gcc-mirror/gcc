-- { dg-do compile }

with Text_IO; use Text_IO;

procedure Dump (A : System.Address; Len : Storage_Offset) is

   Arr : Storage_Array (1 .. Len);
   for Arr'Address use A;
   pragma Import (Ada, Arr);

   H : constant array (Storage_Element range 0 .. 15) of Character :=
         "0123456789abcdef";
begin
   for J in Arr'Range loop
      Put (' ' & H (Arr (J) / 16) & H (Arr (J) mod 16));
   end loop;
end;
