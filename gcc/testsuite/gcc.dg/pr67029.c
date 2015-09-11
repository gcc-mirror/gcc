/* { dg-do compile { target { int128 && scheduling } } } */
/* { dg-options "-O2 -fschedule-insns" } */
/* { dg-additional-options "-fstack-protector" { target fstack_protector } } */

extern void fn2 (char *);
__int128 a, b;
int
fn1 (void)
{
  char e[32];
  fn2 (e);
  b = 9 * (a >> 1);
  return 0;
}
