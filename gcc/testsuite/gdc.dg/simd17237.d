// https://issues.dlang.org/show_bug.cgi?id=17237
// { dg-additional-options "-mavx2" { target avx2_runtime } }
// { dg-do compile { target { avx2_runtime || vect_sizes_32B_16B } } }
import core.simd;

struct S17237
{
    bool a;
    struct
    {
        bool b;
        int8 c;
    }
}

static assert(S17237.a.offsetof == 0);
static assert(S17237.b.offsetof == 32);
static assert(S17237.c.offsetof == 64);
