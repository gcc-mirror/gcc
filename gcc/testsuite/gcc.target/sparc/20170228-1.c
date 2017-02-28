/* PR target/79749 */
/* Reported by Rainer Orth <ro@gcc.gnu.org> */

/* { dg-do run } */
/* { dg-options "-fomit-frame-pointer" } */

extern void abort (void);

int foo (int x1, int x2, int x3, int x4, int x5, int x6, int x7)
{
  return x7;
}

int main (void)
{
  if (foo (100, 200, 300, 400, 500, 600, 700) != 700)
    abort ();

  return 0;
}
