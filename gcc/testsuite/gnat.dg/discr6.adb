-- { dg-do compile }
-- { dg-options "-gnatdm -gnatws" }

with Discr6_Pkg;

procedure Discr6 is

  type T_Bit is range 0..1;
  type T_Entier_16 is range -2**15 .. 2**15-1;

  package My_Q is new Discr6_Pkg(T_Entier_16);

  type T_Valeur is (BIT, Entier_16);

  type R(D : T_Valeur) is record
    case D is
      when BIT => V_BIT : T_Bit;
      when Entier_16 => V_E16 : T_Entier_16;
    end case;
  end record;
  for R use record
    V_BIT at 0 range 0..7;
    V_E16 at 0 range 0..15;
    D     at 8 range 0..7;
  end record;
  for R'size use 128;

  A : R(Entier_16);
  I : Integer;

begin
  I := My_Q.X(A.V_E16);
end;
