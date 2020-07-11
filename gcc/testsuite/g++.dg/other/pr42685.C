// { dg-do compile }
// { dg-options "-O -funroll-loops -fcompare-debug" }

void Remap(int n, int *src, int *dst, int *map)
{
  do {
    int i = *src;
    if (i != 0) *dst = map[i];
  } while (--n != 0);
}
