/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O1 -mlra" } */

#pragma pack(1)
struct
{
  float f0;
} a;

extern void foo (int);

int
main (void)
{
  for (;;)
    foo ((int) a.f0);
}
