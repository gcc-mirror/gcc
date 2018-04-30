-- { dg-do compile }
-- { dg-options "-O" }

package body Array29 is

  procedure Copy (Src : in Matrix; Dst : out Matrix) is
  begin
    for I in Src'Range (1) loop
      for J in Src'Range (2) loop
        Dst (I, J) := Src (I, J);
      end loop;
    end loop;
  end;

  procedure Proc is
    N : constant := 2;
    FM1 : constant Matrix (1 .. N, 1 .. N) := ((1.0, 2.0), (3.0, 4.0));
    FM2 : constant Matrix (1 .. N, 1 .. N) := ((1.0, 2.0), (3.0, 4.0));
    A : constant array (1 .. 2) of Matrix (1 .. N, 1 .. N)
      := (Matrix (FM1), Matrix (FM2));
    Final : Matrix (1 .. N, 1 .. N);
  begin
    Copy (Src => A (1), Dst => Final);
  end;

end Array29;
