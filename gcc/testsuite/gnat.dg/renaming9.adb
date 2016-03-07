-- { dg-do compile }

package body Renaming9 is

  procedure Proc is
  begin
    Obj.I := 0;
  end;

begin
  Obj.I := 0;
end Renaming9;
