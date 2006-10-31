-- { dg-do run }

with Text_IO; use Text_IO;
with GNAT.SPITBOL.Patterns; use GNAT.SPITBOL.Patterns;
procedure Spipaterr is
    X : String := "ABCDE";
    Y : Pattern := Len (1) & X (2 .. 2);
begin
    if Match ("XB", Y) then
       null;
    else
       raise Program_Error;
    end if;
end;
