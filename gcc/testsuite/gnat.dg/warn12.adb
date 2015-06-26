-- { dg-do compile }
-- { dg-options "-O2" }

with Text_IO; use Text_IO;
with System.Storage_Elements; use System.Storage_Elements;
with Warn12_Pkg; use Warn12_Pkg;

procedure Warn12 (N : Natural) is

   Buffer_Size : constant Storage_Offset
      := Token_Groups'Size/System.Storage_Unit + 4096;

   Buffer : Storage_Array (1 .. Buffer_Size);
   for Buffer'Alignment use 8;

   Tg1 : Token_Groups;
   for Tg1'Address use Buffer'Address;

   Tg2 : Token_Groups;
   pragma Warnings (Off, Tg2);

   sid : Sid_And_Attributes;

   pragma Suppress (Index_Check, Sid_And_Attributes_Array);

begin

   for I in 0 .. 7 loop
      sid :=  Tg1.Groups(I);  -- { dg-bogus "out-of-bounds access" }
      Put_Line("Iteration");
   end loop;

   for I in 0 .. N loop
      sid :=  Tg1.Groups(I);  -- { dg-bogus "out-of-bounds access" }
      Put_Line("Iteration");
   end loop;

   for I in 0 .. 7 loop
      sid :=  Tg2.Groups(I);  -- { dg-warning "out-of-bounds access" }
      Put_Line("Iteration");
   end loop;

   for I in 0 .. N loop
      sid :=  Tg2.Groups(I);  -- { dg-warning "out-of-bounds access" }
      Put_Line("Iteration");
   end loop;

end;
