--  { dg-do compile }
--  { dg-options "-gnatX" }

procedure AI12_0086_Example is
    type Enum is (Aa, Bb, Cc, Dd, Ee, Ff, Gg, Hh, Ii, Jj, Kk, Ll, MM,
                  Nn, Oo, Pp, Qq, Rr, Ss, Tt, Uu, Vv, Ww, Xx, Yy, Zz);
    subtype S is Enum range Dd .. Hh;

    type Rec (D : Enum) is record
      case D is
        when S => Foo, Bar : Integer;
        when others => null;
      end case;
    end record;

    function Make (D : S) return Rec is
    begin
      return (D => D, Foo => 123, Bar => 456); -- legal
    end;
begin
    if Make (Ff).Bar /= 456 then
       raise Program_Error;
    end if;
end AI12_0086_Example;