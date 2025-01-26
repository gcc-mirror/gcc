/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-ch -fno-tree-ccp -fno-tree-fre" } */

volatile int a;
int b, c, d, e;
int main() {
  int f = 1, g = 1;
h:
  if (!d)
    ;
  else {
    int i = 1;
  j:
    e = 0;
    for (; e < 3; e++) {
      if (e)
        for (; g < 2; g++) {
          if (c)
            return 0;
          if (f)
            goto j;
        }
      a;
      if (i)
        continue;
      f = i = 0;
    }
  }
  f = 2;
  b++;
  if (c)
    goto h;
  return 0;
}
