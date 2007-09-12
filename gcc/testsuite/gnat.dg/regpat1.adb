--  { dg-do run }

with GNAT.Regpat; use GNAT.Regpat;
procedure regpat1 is
begin
   declare
      Re : Pattern_Matcher := Compile ("a[]b");
   begin
      raise Program_Error;
   end;
exception
   when Expression_Error => null;
end regpat1;
