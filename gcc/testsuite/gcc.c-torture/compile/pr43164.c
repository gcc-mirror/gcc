struct S0
{
  unsigned char f0;
  int:0;
};

struct S1
{
  struct S0 f0;
};

struct S1 func_34 (void)
{
  struct S1 l_221 = { { 1 } };
  return l_221;
}
