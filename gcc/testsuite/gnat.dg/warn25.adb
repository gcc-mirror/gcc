--  { dg-do compile }
--  { dg-options "-gnatwa" }
--  { dg-xfail-if "expected regression" { *-*-* } }

with Ada.Exceptions;
procedure Warn25 is
    CASA_Unavailable : Ada.Exceptions.Exception_Occurrence;
    use Ada.Exceptions;
begin
   while True loop
    declare
    begin
       if Exception_Identity (CASA_Unavailable) = Null_Id then
         exit;
     end if;
     exception
       when E : others =>
         Save_Occurrence (Source => E, Target => CASA_Unavailable);
     end;
   end loop;
   if Exception_Identity (CASA_Unavailable) /= Null_Id then
      Reraise_Occurrence (CASA_Unavailable);
   end if;
end;
