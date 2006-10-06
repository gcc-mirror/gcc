-- { dg-do compile }
-- { dg-options "-O2 -gnatp -Wuninitialized" }

procedure Forward_Vla is

   function N return Natural is begin return 1; end;

   type Sequence;
   type Sequence_Access is access all Sequence;

   Ptr : Sequence_Access := null;  -- freeze access type

   Sequence_Length : Natural := N;
   type Sequence is array (1 .. Sequence_Length) of Natural;

   Seq : Sequence;
begin
   Seq (1) := 0;
end;

