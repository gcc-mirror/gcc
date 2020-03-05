-- { dg-do compile }
-- { dg-options "-O" }

package body Opt79 is

  function F (I : Integer) return Arr is
    A : Arr;

    procedure Nested is

      procedure Inner is
      begin
        A (1) := 0;
      end;

    begin
       Inner;
    end;

  begin
    Nested;
    for J in A'Range loop
      A (J) := I;
    end loop;
    return A;
  end;

end Opt79;
