/* PR target/128911 */
/* { dg-do run { target { { ! ia32 } && { ! *-*-darwin* } } } } */
/* { dg-require-effective-target apxf } */
/* { dg-options "-O2 -mapxf" } */

#define F(n, e) \
  __attribute__((noipa)) int \
  n (int x, unsigned a, unsigned b) { unsigned t = a - b; return e; }

F (f1, x == 0 && (int) t < 0)
F (f2, (int) t < 0 && x == 0)
F (f3, x == 0 && (int) t >= 0)
F (f4, (int) t >= 0 && x == 0)
F (f5, x != 0 || (int) t < 0)

int
main ()
{
  if (!__builtin_cpu_supports ("apxf"))
    return 0;

  unsigned a = 0x80000000u, b = 1;
  if (f1 (0, a, b) != 0 || f2 (0, a, b) != 0 || f3 (0, a, b) != 1
      || f4 (0, a, b) != 1 || f5 (0, a, b) != 0)
    __builtin_abort ();
  return 0;
}
