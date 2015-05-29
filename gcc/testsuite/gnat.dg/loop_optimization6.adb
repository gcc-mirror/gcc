-- { dg-do compile }
-- { dg-options "-O2 -gnatp -fdump-tree-optimized" }

package body Loop_Optimization6 is
  procedure Foo is
  begin
    for I in 1 .. 1_000_000 loop
      A := A + 1;
    end loop;
  end Foo;

  procedure Bar is
  begin
    for J in 1 .. 1_000 loop
      Foo;
    end loop;
  end Bar;

  procedure Main is
  begin
    Bar;
  end;
end Loop_Optimization6;

-- { dg-final { scan-tree-dump-not "goto" "optimized"} }
