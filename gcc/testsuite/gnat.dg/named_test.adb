-- { dg-do run }

with Text_IO; use Text_IO;
procedure Named_Test is
   type Base is tagged limited record
      Flag  : boolean;
      Value : integer;
   end record;
-- 
   function Build (X : Integer; Y : Integer) return Base is
   begin
      return Result : Base do
         Result.Flag := (X = Y);
         Result.Value := X * Y;
      end return;
   end;
-- 
   type Table is array (1..1) of Base;
   It : Table := (1 => Build ( Y => 17, X => 11));
begin
  if It (1).Flag
    or else It (1).Value /= 187
  then
     raise Program_Error;
  end if;
end;
