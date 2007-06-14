--  { dg-do compile }

procedure str1 is
   Str : constant string := "--";
   generic
   package Gen is
      procedure P;
   end Gen;
   package body Gen is
      procedure P is
         inner : String := Str;
      begin
         null;
      end;
   end Gen;
   
   package Inst is new Gen;
begin
   null;
end;
