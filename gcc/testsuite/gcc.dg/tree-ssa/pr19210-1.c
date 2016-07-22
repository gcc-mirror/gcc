/* { dg-do compile } */
/* { dg-options "-O2 -Wunsafe-loop-optimizations" } */
extern void g(void);

void
f (unsigned n)
{
  unsigned k;
  for(k = 0;k <= n;k++) /* { dg-warning "missed loop optimization.*overflow" } */
    g();

  for(k = 0;k <= n;k += 4) /* { dg-warning "missed loop optimization.*overflow" } */
    g();

  /* We used to get warning for this loop.  However, since then # of iterations
     analysis improved, and we can now prove that this loop does not verflow.
     This is because the only case when it would overflow is if n = ~0 (since
     ~0 is divisible by 5), and this cannot be the case, since when we got
     here, the previous loop exited, thus there exists k > n.  */
  for(k = 5;k <= n;k += 5)
    g();

  /* So we need the following loop, instead.  */
  for(k = 4;k <= n;k += 5) /* { dg-warning "missed loop optimization.*overflow" } */
    g();
  
  for(k = 15;k >= n;k--) /* { dg-warning "missed loop optimization.*overflow" } */
    g();
}
