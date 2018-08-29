package Discr54_Pkg is

  type E_TYPE is (A_KIND, B_KIND, C_KIND, X_KIND);

  type DR0 (V : E_TYPE) is tagged
    record
      I0 : INTEGER;
      case V is
        when A_KIND | B_KIND =>
          I1 : INTEGER;
          I2 : INTEGER;
        when C_KIND | X_KIND =>
          I3 : INTEGER;
      end case;
    end record;

  type DN1 is new DR0 (V => A_KIND) with
    record
      I4 : INTEGER;
      I5 : INTEGER;
    end record;

  type DR1 (W : E_TYPE) is new DR0 (V => A_KIND) with
    record
      I4 : INTEGER;
      case W is
        when A_KIND | B_KIND =>
          I5 : INTEGER;
        when C_KIND | X_KIND =>
          I6 : INTEGER;
      end case;
    end record;

  type DD1 (W : E_TYPE; V : E_TYPE) is new DR0 (V => V) with
    record
      I4 : INTEGER;
      case W is
        when A_KIND | B_KIND =>
          I5 : INTEGER;
        when C_KIND | X_KIND =>
          I6 : INTEGER;
      end case;
    end record;

  type DR2 is new DR1 (W => A_KIND) with
    record
      I7 : INTEGER;
    end record;

  V0 : constant DR0 := DR0'(I0 => 0,
                            V  => A_KIND,
                            I1 => 0,
                            I2 => 0
                           );

  N1 : constant DN1 := DN1'(V  => A_KIND,
                            I0 => 0,
                            I1 => 0,
                            I2 => 0,
                            I4 => 0,
                            I5 => 0);

  N2 : constant DN1 := DN1'(V  => A_KIND,
                            I0 => 0,
                            I1 => 0,
                            I2 => 0,
                            I4 => 0,
                            I5 => 0);

  D1 : constant DD1 := DD1'(W  => A_KIND,
                            V  => A_KIND,
                            I0 => 0,
                            I1 => 0,
                            I2 => 0,
                            I4 => 0,
                            I5 => 0);

  V1 : constant DR1 := DR1'(W  => A_KIND,
                            I0 => 0,
                            I1 => 0,
                            I2 => 0,
                            I4 => 0,
                            I5 => 0);

  V3 : constant DR2 := DR2'(V1 with I7 => 0);
  V2 : constant DR2 := DR2'(
                            --  V  => A_KIND,  --  needed???
                            W  => A_KIND,
                            I0 => 0,
                            I1 => 1,
                            I2 => 2,
                            I4 => 4,
                            I5 => 5,
                            I7 => 7);

  pragma Assert (
      V2.I0 = 0
    and then V2.I1 = 1
    and then V2.I1 = 1
    and then V2.I2 = 2
    and then V2.I4 = 4
    and then V2.I5 = 5
    and then V2.I7 = 1);

end Discr54_Pkg;
