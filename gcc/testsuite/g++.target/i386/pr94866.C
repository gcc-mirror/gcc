// PR target/94866
// { dg-do compile }
// { dg-options "-O2 -msse4.1" }
// { dg-require-effective-target c++11 }

typedef long long v2di __attribute__((vector_size(16)));

v2di _mm_move_epi64(v2di a)
{
    return v2di{a[0], 0LL};
}

// { dg-final { scan-assembler-times "movq\[ \\t\]+\[^\n\]*%xmm" 1 } }
