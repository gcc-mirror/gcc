-- { dg-do compile }
-- { dg-options "-O2" }

PROCEDURE Array8 IS

  function ID (I : Integer) return Integer is
  begin
    return I;
  end;

  SUBTYPE STB IS INTEGER RANGE ID(-8) .. -5;

  TYPE TB IS ARRAY (STB RANGE <>) OF INTEGER;

  GENERIC
    B1 : TB;
  PROCEDURE PROC1;

  PROCEDURE PROC1 IS
  BEGIN
    IF B1'FIRST /= -8 THEN
      raise Program_Error;
    ELSIF B1'LAST /= ID(-5) THEN
      raise Program_Error;
    ELSIF B1 /= (7, 6, 5, 4) THEN
      raise Program_Error;
    END IF;
  END;

  PROCEDURE PROC2 IS NEW PROC1 ((7, 6, ID(5), 4));

BEGIN
  PROC2;
END;
