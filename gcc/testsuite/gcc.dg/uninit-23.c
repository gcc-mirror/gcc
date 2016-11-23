/* PR tree-optimization/78455 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int ij;

void
ql (void)
{
  int m5 = 0;

  for (;;)
  {
    if (0)
      for (;;)
      {
        int *go;
        int *t4 = go;

 l1:
        *t4 = (*t4 != 0) ? 0 : 2; /* { dg-warning "may be used uninitialized" } */
      }

    if (ij != 0)
      goto l1;
  }
}
