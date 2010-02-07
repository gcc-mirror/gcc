static short foo (long long si1, short si2)
{
  return si1 > 0 && si2 > 0 || si1 < 0
      && si2 < 0 && si1 < 1 - si2 ? : si1 + si2;
}

int g_13;
unsigned g_17;

int safe (int, int);

void bar (short p_51, short * p_52)
{
  int *const l_55 = &g_13;
  if (safe (*p_52, g_13 != foo (*p_52 & *l_55 == g_13 && g_17 >= 1, 0)))
    {
    }
}
