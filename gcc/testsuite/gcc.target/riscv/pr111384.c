/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-ext_dce" } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */

void
foo(unsigned int src, unsigned short *dst1, unsigned short *dst2)
{
    *dst1 = src;
    *dst2 = src;
}

