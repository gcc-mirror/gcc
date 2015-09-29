/* PR target/pr65105 */
/* { dg-do run { target { ia32 } } } */
/* { dg-options "-O2 -march=slm" } */

struct s {
  long long l1, l2, l3, l4, l5;
} *a;
long long b;
long long fn1()
{
  try
    {
      b = (a->l1 | a->l2 | a->l3 | a->l4 | a->l5);
      return a->l1;
    }
  catch (int)
    {
    }
}
