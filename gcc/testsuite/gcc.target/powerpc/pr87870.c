/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2" } */

__int128
test0 (void)
{
  return 0;
}

__int128
test1 (void)
{
  return 1;
}

__int128
test2 (void)
{
  return -1;
}

__int128
test3 (void)
{
  return ((__int128)0xdeadbeefcafebabe << 64) | 0xfacefeedbaaaaaad;
}

/* { dg-final { scan-assembler-not {\mld\M} } } */
