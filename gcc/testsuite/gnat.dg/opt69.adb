-- { dg-do compile }
-- { dg-options "-O" }

with Ada.Text_IO;

procedure Opt69 is

   procedure Inner
     (A : String := (1 .. 15 => ASCII.NUL);
      B : String := (1 .. 5 => ASCII.NUL);
      C : String := (1 .. 5 => ASCII.NUL))
   is
      Aa : String (1 .. 15);
      Bb : String (1 .. 5);
      Cc : String (1 .. 5);
   begin
      Aa := A;
      Bb := B;
      Cc := C;

      Ada.Text_IO.Put_Line (Aa);
      Ada.Text_IO.Put_Line (Bb);
      Ada.Text_IO.Put_Line (Cc);
   end;

begin
   Inner;
end;
