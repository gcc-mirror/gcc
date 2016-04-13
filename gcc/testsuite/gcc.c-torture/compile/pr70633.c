/* PR middle-end/70633 */

typedef long V __attribute__((vector_size (4 * sizeof (long))));

void foo (V *);

void
bar (void)
{ 
  V b = { (long) bar, 0, 0, 0 };
  foo (&b);
}
