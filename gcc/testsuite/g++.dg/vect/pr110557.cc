// { dg-additional-options "-mavx" { target { avx_runtime } } }

typedef long long i64;

static inline i64
min (i64 a, i64 b)
{
  return a < b ? a : b;
}

struct Item
{
  int x : 8;
  i64 y : 55;
  bool z : 1;
};

__attribute__ ((noipa)) i64
test (Item *a, int cnt)
{
  i64 size = 0;
  for (int i = 0; i < cnt; i++)
    size = min (a[i].y, size);
  return size;
}

int
main ()
{
  struct Item items[] = {
    { 1, -1 },
    { 2, -2 },
    { 3, -3 },
    { 4, -4 },
  };

  if (test (items, 4) != -4)
    __builtin_trap ();
}
