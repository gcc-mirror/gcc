/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-require-effective-target dfp } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-O2 -mdejagnu-cpu=G5 -maltivec" } */

union ieee754r_Decimal32
{
  _Decimal32 sd;
  unsigned int cc0;
};

unsigned int
__decoded32 (_Decimal32 a)
{
    union ieee754r_Decimal32 d;
    d.sd = a;
    return d.cc0;
}
