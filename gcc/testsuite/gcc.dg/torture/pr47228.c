/* { dg-do run } */

struct S4
{
  unsigned f0:24;
} __attribute__((__packed__));

struct S4 g_10 = {
  6210831
};

struct S4 func_2 (int x)
{
  struct S4 l_8[2] = {
    {0}, {0}
  };
  g_10 = l_8[1];
  for (; x<2; x++) {
    struct S4 tmp = {
      11936567
    };
    l_8[x] = tmp;
  }
  return g_10;
}

int main (void)
{
  func_2 (0);
  return 0;
}
