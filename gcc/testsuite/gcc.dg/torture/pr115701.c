/* { dg-do run } */
/* IPA PTA disables local PTA recompute after IPA.  */
/* { dg-additional-options "-fipa-pta" } */

int a, c, d;
static int b;
int main()
{
  int *e = &a, **f = &e;
  while (1) {
    int **g, ***h = &f;
    if (c)
      *g = e;
    else if (!b)
      break;
    *e = **g;
    e = &d;
  }
  if (e != &a)
    __builtin_abort();
  return 0;
}
