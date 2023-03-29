// https://issues.dlang.org/show_bug.cgi?id=23085
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

float test23085(float x)
{
    byte i = *cast(byte*)&x;
    ++i;
    return *cast(float*)&i; // this cast is not allowed in @safe code
}
