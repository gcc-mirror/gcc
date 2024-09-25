/* CTF is not generated for entities not at file-scope.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "ascii \"SFOO.0\"\[\t \]+\[^\n\]*ctf_string" 0 } } */
/* { dg-final { scan-assembler-times "ascii \"gfoo.0\"\[\t \]+\[^\n\]*ctf_string" 0 } } */
/* { dg-final { scan-assembler-times "ascii \"foo.0\"\[\t \]+\[^\n\]*ctf_string" 1 } } */

int foo (int n)
{
  typedef struct { int a[6]; } SFOO;

  SFOO a;
  __attribute__ ((noinline)) SFOO gfoo (void) { return a; }

  a.a[0] = 1;
  a.a[9] = 2;

  SFOO b;
  b = gfoo ();

  return b.a[0] == 1 && b.a[9] == 2;
}

