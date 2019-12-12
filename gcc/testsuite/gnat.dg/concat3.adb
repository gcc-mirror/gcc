--  { dg-do run }
--  { dg-options "-g -O0 -gnata" }

procedure Concat3 is
   procedure Show_Bug (S : in String)
   is
      Str : constant String := S & "-" with Alignment => 4;
   begin
      null;
   end Show_Bug;

begin
   Show_Bug ("BUG");
end Concat3;