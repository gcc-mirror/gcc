-- { dg-do compile }

with Pack3_Pkg;

package Pack3 is

  subtype N_TYPE is INTEGER range 0..5;

  type LIST_ARRAY is array (N_TYPE range <>) of INTEGER;

  type LIST (N : N_TYPE := 0) is record
    LIST : LIST_ARRAY(1..N);
  end record;
  pragma PACK(LIST);

  subtype CS is STRING(1..Pack3_Pkg.F);

  type CSA is array (NATURAL range <>) of CS;

  type REC is record
    I1, I2 : INTEGER;
  end record ;

  type CMD is (CO, AS);

  type CMD_BLOCK_TYPE (D : CMD := CO) is record
    N : CSA (1..4);
    case D is
      when CO => L : LIST;
      when AS => R : REC;
    end case ;
  end record;
  pragma PACK(CMD_BLOCK_TYPE);

  type CMD_TYPE is (RIGHT, WRONG);

  type CMD_RESULT (D : CMD_TYPE) is record
    case D is
      when RIGHT => C : CMD_BLOCK_TYPE;
      when WRONG => null;
    end case;
  end record ;
  pragma PACK(CMD_RESULT);

end Pack3;
