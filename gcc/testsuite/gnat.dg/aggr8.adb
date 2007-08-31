--  { dg-do compile }

procedure aggr8 is
   
   type Byte is mod 2 ** 8;
   subtype two is integer range 1..2;
   -- type Sequence is array (1 .. 2) of Byte;
   type Sequence is array (Two) of Byte;
   
   type Block is record
      Head : Sequence  := (11, 22);
   end record;
   
   procedure Nest is
      Blk : Block;  pragma Unreferenced (Blk);
   begin
      null;
   end;

begin
   null;
end;
