// https://issues.dlang.org/show_bug.cgi?id=23218
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do run { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

__vector(int[4]) convtest(int[4] a)
{
    return cast(__vector(int[4]))a;
}

void main()
{
    static assert(convtest([1,2,3,4])[0] == 1);
    assert(convtest([1,2,3,4])[0] == 1);
}
