--  { dg-do compile }
--  { dg-options "-gnatf" }

procedure Unreferenced2 is

   protected Example is
      procedure Callme;
   end Example;

   procedure Other (X : Boolean) is
   begin
      null;
   end;

   protected body Example is

      procedure Internal (X : Boolean) is
         pragma Unreferenced (X);
        Y : Integer;
      begin
         Y := 3;
      end Internal;

      procedure Callme is
      begin
         Internal (X => True);
      end Callme;

   end Example;

begin
   Example.Callme;
   Other (True);
end Unreferenced2;
