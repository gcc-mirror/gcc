/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv64gc_zba -O2 -mabi=lp64" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**   ...
**   slli\s*a1,a1,1
**   add\s*a0,a1,a0
**   ...
*/
/* { dg-final { scan-assembler ".option arch, rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0" } } */
long foo (long a, long b) __attribute__((target("arch=rv64gc")));
long foo (long a, long b)
{
  return a + (b * 2);
}

/*
** bar:
**   ...
**   sh1add\s*a0,a1,a0
**   ...
*/
long bar (long a, long b)
{
  return a + (b * 2);
}
