/* PR debug/46095 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -fschedule-insns2 -fno-omit-frame-pointer -fstack-protector" } */

extern void bar (char *);

void
foo (void)
{
  char c[0x80000000UL];
  bar (c);
}
