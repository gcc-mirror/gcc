/* { dg-options "-fno-common" { target { hppa*-*-hpux* } } } */
#include "pr83487-1.h"

extern
#ifdef __cplusplus
"C"
#endif
void abort ();

void
f1 (int i, int j, int k, int l, int m, int n, int o, struct A x)
{
  if (i != 6 || j != 0 || k != 1 || l != 2 || m != 3 || n != 4 || o != 5)
    abort ();
}

void
f2 (int i, int j, int k, int l, int m, int n, int o, struct A x, int p, int q)
{
  if (i != 6 || j != 0 || k != 1 || l != 2 || m != 3 || n != 4 || o != 5 || p != 7 || q != 8)
    abort ();
}

void
f3 (int i, int j, int k, int l, int m, int n, int o, struct B x, int p, int q)
{
  if (i != 6 || j != 0 || k != 1 || l != 2 || m != 3 || n != 4 || o != 5 || p != 7 || q != 8)
    abort ();
}

void
f4 (int i, int j, int k, int l, int m, int n, int o, struct C x, int p, int q)
{
  if (i != 6 || j != 0 || k != 1 || l != 2 || m != 3 || n != 4 || o != 5 || p != 7 || q != 8)
    abort ();
}

void
f5 (int o, struct A x)
{
  if (o != 5)
    abort ();
}

void
f6 (int o, struct A x, int p, int q)
{
  if (o != 5 || p != 7 || q != 8)
    abort ();
}

void
f7 (int o, struct B x, int p, int q)
{
  if (o != 5 || p != 7 || q != 8)
    abort ();
}

void
f8 (int o, struct C x, int p, int q)
{
  if (o != 5 || p != 7 || q != 8)
    abort ();
}
