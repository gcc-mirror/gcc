-- { dg-do compile }

procedure Addr6 is

   type Byte is mod 2**8;

   type Byte_Arr1 is array (Positive range <>) of Byte;
   for Byte_Arr1'Alignment use 4;

   type Byte_Arr2 is array (Positive range <>) of Byte;

   function Length return Natural is
   begin
     return 1;
   end;

   function Empty return Byte_Arr2 is
     Null_Arr : Byte_Arr2 (1 .. 0);
   begin
     return Null_Arr;
   end;

   A1 : Byte_Arr1 (1 .. Length);

   A2 : Byte_Arr2 (A1'Range);
   for A2'Alignment use 4;
   for A2'Address use A1'Address;

begin
   A2 := Empty;
end;
