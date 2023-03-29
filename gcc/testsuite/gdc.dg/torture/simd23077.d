// https://issues.dlang.org/show_bug.cgi?id=23077
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

float test23077(float x)
{
    short i = *cast(short*)&x;
    ++i;
    return *cast(float*)&i; // this cast is not allowed in @safe code
}
