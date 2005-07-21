/* { dg-do compile } */
/* { dg-options "-O2 -funsafe-loop-optimizations -Wunsafe-loop-optimizations" } */
extern void g(void);

void
f (unsigned n)
{
  unsigned k;
  for(k = 0;k <= n;k++) /* { dg-warning "assuming.*not infinite" } */
    g();

  for(k = 5;k <= n;k += 4) /* { dg-warning "assuming.*not overflow" } */
    g();

  for(k = 5;k <= n;k += 5) /* { dg-warning "assuming.*not overflow" } */
    g();

  for(k = 15;k >= n;k--) /* { dg-warning "assuming.*not infinite" } */
    g();

}
