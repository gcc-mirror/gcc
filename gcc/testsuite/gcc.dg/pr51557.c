/* PR debug/51557 */
/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-Os -fno-asynchronous-unwind-tables -g -fsel-sched-pipelining -fselective-scheduling2" } */

extern int baz (void);
extern void bar (int, int, int, int, int, int, int);

void
synth (int *values, int n_values, int ci, int s1, int v, int s2)
{
  while (--s1)
    {
      int r1 = values[s1];
      int co = ci ? r1 : baz () < r1;
      bar (0, n_values, s1, s2, v, co, 0);
    }
}
