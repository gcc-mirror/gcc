// { dg-additional-options "-mavx" { target { avx_runtime } } }

static inline long
min (long a, long b)
{
  return a < b ? a : b;
}

struct Item
{
  int x : 8;
  long y : 55;
  bool z : 1;
};

__attribute__ ((noipa)) long
test (Item *a, int cnt)
{
  long size = 0;
  for (int i = 0; i < cnt; i++)
    size = min ((long)a[i].y, size);
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
