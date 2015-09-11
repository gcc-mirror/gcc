-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

with Noinline3_Pkg;

package Noinline3 is new Noinline3_Pkg (0);

-- { dg-final { scan-tree-dump-times "noinline3.inner" 2 "optimized"  } }
