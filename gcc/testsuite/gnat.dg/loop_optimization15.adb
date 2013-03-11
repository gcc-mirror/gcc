-- { dg-do compile }
-- { dg-options "-O3" }

package body Loop_Optimization15 is

  type Integer_Array_T is array (B16_T range <>) of Integer;

  Len : constant B16_T := 10;

  Src : constant Integer_Array_T (1 .. Len) := (others => 0);
  Dst : Integer_Array_T (1 .. Len);

  procedure Proc (L : B16_T) is
  begin
      for I in  1 .. B16_T'Min (L, Len) loop
          Dst (I) := Src (I);
      end loop;
  end;

end Loop_Optimization15;
