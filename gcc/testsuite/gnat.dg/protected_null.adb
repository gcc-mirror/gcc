--  { dg-do compile }

procedure Proc is
   protected Po is
      procedure P is null;  --  { dg-error " protected operation cannot be a null procedure" }
   end Po;
   protected body Po is
      procedure P is
      begin
         null;
      end P;
   end Po;
begin
   null;
end;
