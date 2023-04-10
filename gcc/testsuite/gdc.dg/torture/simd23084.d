// https://issues.dlang.org/show_bug.cgi?id=23084
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

__vector(int[4]) test23084a(__vector(int[4]) a)
{
    __vector(short[8]) r = cast(short)(a.array[0]);
    return cast(__vector(int[4]))r;
}

__vector(int[4]) test23084b(__vector(int[4]) a)
{
    __vector(byte[16]) r = cast(byte)(a.array[0]);
    return cast(__vector(int[4]))r;
}
