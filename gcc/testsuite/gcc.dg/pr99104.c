/* PR target/99104 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fsel-sched-pipelining -fselective-scheduling2 -funroll-loops" } */

__int128 a;
int b;
int foo (void);

int __attribute__ ((simd))
bar (void)
{
  a = ~a;
  if (foo ())
    b = 0;
}
