/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-ch -fno-tree-ccp -fno-tree-fre" } */

volatile int a;
int b, c, d, e, f, g;
int main() {
  int i = 2, j = 1;
k:
  if (!e)
    ;
  else {
    short l = 1;
    if (0)
    m:
      d = g;
    f = 0;
    for (; f < 2; f++) {
      if (f)
        for (; j < 2; j++)
          if (i)
            goto m;
      a;
      if (l)
        continue;
      i = 0;
      while (c)
        l++;
    }
    g = 0;
  }
  if (b) {
    i = 1;
    goto k;
  }
  return 0;
}
