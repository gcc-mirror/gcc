/* PR target/82112 */
/* { dg-do compile } */

int c[10], d[10], e[10], f[10], g[10], h[10], i[10], j[10], k[10], l[10];

void
foo (void)
{
  __atomic_load (c, d, __ATOMIC_ACQUIRE);
  __atomic_store (e, f, __ATOMIC_SEQ_CST);
  __atomic_exchange (g, h, i, __ATOMIC_RELAXED);
  __atomic_compare_exchange (j, k, l, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}
