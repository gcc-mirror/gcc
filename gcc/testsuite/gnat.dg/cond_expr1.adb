-- { dg-do compile }
-- { dg-options "-gnat12" }

function Cond_Expr1 (Dir : in String) return String is
begin
   return (if Dir (Dir'Last) = '\' then Dir else Dir & '\');
end;
