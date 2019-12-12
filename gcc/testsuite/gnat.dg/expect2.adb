-- { dg-do compile }

with Text_IO; use Text_IO;
with Expect2_Pkg; use Expect2_Pkg;

procedure Expect2 is
begin
  if Unlikely (I = 0) then
    Put_Line ("Zero was passed");
    return;
  end if;

  if Likely (I > 0) then
    Put_Line ("A positive number was passed");
  else
    Put_Line ("A negative number was passed");
  end if;

  if Expect ((I rem 2) = 0, False) then
    Put_Line ("An even number was passed");
  else
    Put_Line ("An odd number was passed");
  end if;
end;
