/* { dg-do compile }  */
/* { dg-options "-O1 -fno-tree-ccp -fno-tree-dse -Wmaybe-uninitialized" } */

int oo;

void
pc (int *tt)
{
  int cf = 0;

  if (*tt != 0)
    {
      if (0)
        {
          int *qg;
          int uj = 0;

 t6:
          tt = &cf;
          if (oo != 0)
            {
              ++uj; /* { dg-warning "may be used uninit" } */
              *qg = !!oo && !!uj; /* { dg-warning "may be used uninit" } */
            }
        }
      cf = 0;
      goto t6;
    }

  if (oo != 0)
    {
      *tt = 1;
      goto t6;
    }
}
