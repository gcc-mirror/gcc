/* { dg-do compile } */
/* { dg-options "-fext-dce -fdump-rtl-ext_dce" } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */
/* { dg-skip-if "" { *-*-* } { "-O0"} } */

void
foo(unsigned int src, unsigned short *dst1, unsigned short *dst2)
{
    *dst1 = src;
    *dst2 = src;
}

