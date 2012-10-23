/* Test AAPCS layout

/* { dg-do compile { target aarch64*-*-* } } */

__complex__ long int
ctest_long_int(__complex__ long int x)
{
    return x;
}
