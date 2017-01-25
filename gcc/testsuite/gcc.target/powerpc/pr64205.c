/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-skip-if "" { powerpc*-*-aix* } { "*" } { "" } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=G5" } } */
/* { dg-options "-O2 -mcpu=G5 -maltivec" } */

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
