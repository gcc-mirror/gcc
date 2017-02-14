-- { dg-do run }
-- { dg-options "-O" }

with Opt59_Pkg; use Opt59_Pkg;

procedure Opt59 is

  type Enum is (Zero, One, Two);

  function Has_True (V : Boolean_Vector) return Boolean is
  begin
     for I in V'Range loop
        if V (I) then
           return True;
        end if;
     end loop;
     return False;
  end;

  Data1  : constant Boolean_Vector := Get_BV1;
  Data2  : constant Boolean_Vector := Get_BV2;
  Result : Boolean_Vector;

  function F return Enum is
    Res  : Enum := Zero;
    Set1 : constant Boolean := Has_True (Data1);
    Set2 : constant Boolean := Has_True (Data2);
  begin
    if Set1 then
      Res := Two;
    elsif Set2 then
      Res := One;
    end if;
    return Res;
  end;

  Val : constant Enum := F;

begin

  for I in Result'Range loop
    Result (I) := Data1 (I) or Data2 (I);
  end loop;

  if Val /= Zero then
    Test (Val = Two);
  end if;

end;
