/* PR c/21759  */
/* { dg-options "-Wc++-compat" } */

int
main(void)
{
   void *p = 0;
   int *q = p;                  /* { dg-warning "not permitted" } */
   double* t = (void *)0;
   return p != q;
}
