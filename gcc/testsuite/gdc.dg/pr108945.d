// { dg-options "-fdump-tree-gimple" }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }

alias f4 = __vector(float[4]);

auto pr108945(f4 a, f4 b)
{
    return a < b;
}

// { dg-final { scan-tree-dump-not "VEC_COND_EXPR" "gimple" } }
