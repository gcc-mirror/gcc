// { dg-do compile }
// { dg-options "-O0 -fdump-tree-optimized-raw" }

import gcc.attributes;

double notfast(double x)
{
    return x * x * x * x * x * x * x * x;
}

// { dg-final { scan-tree-dump-times "mult_expr, _" 7 "optimized" } }

@fastmath
static double fast(double x)
{
    return x * x * x * x * x * x * x * x;
}

// { dg-final { scan-tree-dump-times "mult_expr, powmult_" 3 "optimized" } }
