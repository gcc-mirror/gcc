--  { dg-do run }

with System; use System;

procedure Addr15 is

  function Get_Bound (Param : Integer) return Integer is (Param);

  type Alpha_Typ is array (1 .. Get_Bound (1)) of Integer;
  type Beta_Typ  is array (1 .. Get_Bound (0)) of Integer;

  Alpha : Alpha_Typ;
  Beta  : aliased Beta_Typ;

begin
  if Alpha'Address = Beta'Address then
    raise Program_Error;
  end if;
end;
