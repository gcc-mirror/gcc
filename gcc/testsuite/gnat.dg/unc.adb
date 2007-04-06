-- { dg-do compile }

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
procedure Unc is
   type Arr is array (1..4) of integer;
   type Bytes is array (positive range <>) of Character;
   type Buffer (D : Boolean := False) is record
      case D is 
        when False =>
           Chars: Bytes (1..16);
        when True =>
           Values : Arr;
      end case;
   end record;
--
   pragma Unchecked_Union (Buffer);
   pragma Warnings (Off);
   Val : Buffer;
--      
   F : File_Type;
   S : Stream_Access;
begin
   Create (F, Out_File);
   S := Stream (F);
   Buffer'Output (S, Val);
end;
