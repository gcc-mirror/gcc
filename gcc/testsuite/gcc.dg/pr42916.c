/* { dg-do compile } */
/* { dg-options "-O1 -funroll-loops -ftree-vectorize -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

int seed;

static inline int hash(const char *str)
{
  int h = seed++;
  int i = 12;
  while (i--)
    h += (h << 3) ^ *str++;
  return h;
}

void f(const char *str, int *h)
{
  int i = 6;
  while (i--)
    *h++ = hash(str);
}
