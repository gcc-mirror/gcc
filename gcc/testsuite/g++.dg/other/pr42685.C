// { dg-do compile }
// { dg-options "-O -funroll-loops -fcompare-debug" }
// { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } }

void Remap(int n, int *src, int *dst, int *map)
{
  do {
    int i = *src;
    if (i != 0) *dst = map[i];
  } while (--n != 0);
}
