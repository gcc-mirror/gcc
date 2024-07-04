/* { dg-require-effective-target int32plus } */
/* PR tree-optimization/112581 */
/* reassociation, used to combine 2 bb to together,
   that made an unitialized variable unconditional used
   which then at runtime would cause an infinite loop.  */
int a = -1, b = 2501896061, c, d, e, f = 3, g;
int main() {
  unsigned h;
  int i;
  d = 0;
  for (; d < 1; d++) {
    int j = ~-((6UL ^ a) / b);
    if (b)
    L:
      if (!f)
        continue;
    if (c)
      i = 1;
    if (j) {
      i = 0;
      while (e)
        ;
    }
    g = -1 % b;
    h = ~(b || h);
    f = g || 0;
    a = a || 0;
    if (!a)
      h = 0;
    while (h > 4294967294)
      if (i)
        break;
    if (c)
      goto L;
  }
  return 0;
}
