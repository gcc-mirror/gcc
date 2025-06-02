--  { dg-do run }
--  { dg-options "-O1 -fstack-check" }
--  from PR middle-end/118939

with Controlled9_Pkg;
procedure Controlled9 is
   S : constant Controlled9_Pkg.T_Access := new Controlled9_Pkg.T;
begin
   null;
end Controlled9;	
