--  { dg-do run }
--  { dg-options "-gnatws -gnata" }

with GNAT.Random_Numbers;

procedure Aggr27 is

   Gen: GNAT.Random_Numbers.Generator;

   function Random return Long_Long_Integer is
      Rand : Integer := GNAT.Random_Numbers.Random(Gen);
   begin
      return Long_Long_Integer(Rand);
   end Random;

   type Values is range 1 .. 4;

   Seq_LLI : array (Values) of Long_Long_Integer := (others => Random);
   Seq_I   : array (Values) of Integer           := (others => Integer(Random));

begin
   --  Verify that there is at least two  different entries in each.

   pragma Assert (For some E of Seq_LLI => E /= Seq_LLI (Values'First));
   pragma Assert (For some E of Seq_I => E /= Seq_I (Values'First));
end Aggr27;
