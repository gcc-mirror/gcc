/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

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
