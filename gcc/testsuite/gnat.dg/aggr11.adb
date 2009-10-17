-- { dg-do compile }
-- { dg-options "-O" }

with Aggr11_Pkg; use Aggr11_Pkg;

procedure Aggr11 is

  A : Arr := ((1 => (Kind  => No_Error, B => True),
               2 => (Kind => Error),
               3 => (Kind => Error),
               4 => (Kind  => No_Error, B => True),
               5 => (Kind  => No_Error, B => True),
               6 => (Kind  => No_Error, B => True)));

begin
   null;
end;
