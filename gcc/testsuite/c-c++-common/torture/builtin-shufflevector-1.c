/* { dg-do run } */

typedef long v4di __attribute__((vector_size(4 * sizeof (long))));
typedef int v4si __attribute__((vector_size(4 * sizeof (int))));
typedef int v8si __attribute__((vector_size(8 * sizeof (int))));

v4si res[5], a, b;
v4di resl, al, bl;
v8si res8[3], a8, b8;
void __attribute__((noipa))
foo (void)
{
  res[0] = __builtin_shufflevector (a, b, 0, 1, 4, 5);
  res[1] = __builtin_shufflevector (a, b, 0, 1, -1, 5);
  res8[0] = __builtin_shufflevector (a, b, 0, 1, 2, 2 + 1, 4, 5, 6, 7);
  res[2] = __builtin_shufflevector (a8, b8, 0, 8, 1, 9);
  res[3] = __builtin_shufflevector (a8, b, 0, 8, 1, 9);
  res[4] = __builtin_shufflevector (a, b8, 0, 4, 1, 5);
  res8[1] = __builtin_shufflevector (a8, b, 0, 8, 1, 9, 10, 11, 2, 3);
  res8[2] = __builtin_shufflevector (a, b8, 0, 4, 1, 5, -1, -1, -1, -1);
}

#define comp(a, b, n) \
  for (unsigned i = 0; i < n; ++i) \
    if ((a)[i] != (b)[i]) \
      __builtin_abort ();

int main()
{
  a = (v4si) { 0, 1, 2, 3 };
  b = (v4si) { 4, 5, 6, 7 };
  a8 = (v8si) { 0, 1, 2, 3, 4, 5, 6, 7 };
  b8 = (v8si) { 8, 9, 10, 11, 12, 13, 14, 15 };
  foo ();
  comp (res[0], ((v4si) { 0, 1, 4, 5}), 4)
  res[1][2] = 9;
  comp (res[1], ((v4si) { 0, 1, 9, 5}), 4)
  comp (res8[0], ((v8si) { 0, 1, 2, 3, 4, 5, 6, 7 }), 8)
  comp (res[2], ((v4si) { 0, 8, 1, 9}), 4)
  comp (res[3], ((v4si) { 0, 4, 1, 5}), 4)
  comp (res[4], ((v4si) { 0, 8, 1, 9}), 4)
  comp (res8[1], ((v8si) { 0, 4, 1, 5, 6, 7, 2, 3 }), 8)
  res8[2][4] = 42;
  res8[2][5] = 42;
  res8[2][6] = 42;
  res8[2][7] = 42;
  comp (res8[2], ((v8si) { 0, 8, 1, 9, 42, 42, 42, 42 }), 8)
  return 0;
}
