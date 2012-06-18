-- { dg-do compile }

with System; use System;

procedure Array22 is

   type Integer_Address is mod Memory_Size;

   type Memory is array (Integer_Address range <>) of Character;

   type Chunk (First, Last : Integer_Address) is record
      Mem : Memory (First .. Last);
   end record;

   C : Chunk (1, 8);
   for C'Alignment use 8;
   pragma Unreferenced (C);

begin
   null;
end;
