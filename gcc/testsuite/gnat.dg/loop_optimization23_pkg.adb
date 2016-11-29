-- { dg-do compile }
-- { dg-options "-O3" }
-- PR tree-optimization/71083
package body Loop_Optimization23_Pkg is
  procedure Foo (X : in out ArrayOfStructB) is
  begin
    for K in 0..99 loop
      X (K+1).b.b := X (K).b.b;
    end loop;
  end Foo;
end Loop_Optimization23_Pkg;
