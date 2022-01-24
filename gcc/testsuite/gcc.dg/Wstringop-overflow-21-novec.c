/* PR middle-end/92312 - bogus -Wstringop-overflow storing into a trailing
   array backed by larger buffer
   { dg-do compile }
   { dg-options "-O2 -fno-tree-vectorize -Wall -Wno-array-bounds" } */

struct S0 { char a, b[0]; };

void sink (void*);

void test_store_zero_length (int i)
{
  char a[3];
  struct S0 *p = (struct S0*)a;
  p->a = 0;
  p->b[0] = 0;
  p->b[1] = 1;                      // { dg-bogus "\\\[-Wstringop-overflow" }
  p->b[2] = 2;                      // { dg-warning "\\\[-Wstringop-overflow" }
  p->b[i] = 2;
  sink (p);
}

struct Sx { char a, b[]; };

void test_store_flexarray (int i)
{
  char a[3];
  struct Sx *p = (struct Sx*)a;
  p->a = 0;
  p->b[0] = 0;
  p->b[1] = 1;                      // { dg-bogus "\\\[-Wstringop-overflow" }
  p->b[2] = 1;                      // { dg-warning "\\\[-Wstringop-overflow" }
  p->b[i] = 2;
  sink (p);
}
