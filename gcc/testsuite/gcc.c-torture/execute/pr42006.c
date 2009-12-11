extern void abort (void);

static unsigned int
my_add(unsigned int si1, unsigned int si2)
{
  return (si1 > (50-si2)) ? si1 : (si1 + si2);
}

static unsigned int
my_shift(unsigned int left, unsigned int right)
{
  return  (right > 100) ? left : (left >> right);
}

static int func_4(unsigned int p_6)
{
  int count = 0;
  for (p_6 = 1; p_6 < 3; p_6 = my_add(p_6, 1))
    {
      if (count++ > 1)
	abort ();

      if (my_shift(p_6, p_6))
	return 0;
    }
  return 0;
}

int main(void)
{
  func_4(0);
  return 0;
}
