static int
foo (int si1, int si2)
{
  return si1 > 0 && si2 > 0 && si1 > -si2 || si1 < 0 && si2 < 0
    && si1 < -si2 ? : si1 + si2;
}

struct S0
{
  unsigned short f1;
};
int g_4;
struct S0 g_54 = {
  3428
};

int
func_21 (int * p_22, int * const int32p_24, unsigned p_25,
         const int * p_26);

void int324 (unsigned p_15, int * p_16, int * p_17, int * p_18)
{
  if (foo (g_4, func_21 (p_18, &g_4, 0, 0)))
    {
      for (g_54.f1; g_54.f1; g_54.f1 += 1)
        {
        }
    }
}

int
func_21 (int * p_22, int * const int32p_24, unsigned p_25,
         const int * p_26)
{
  for (0; 1; p_25 += 1)
  lbl_29:if (p_25)
      goto lbl_28;
lbl_28:for (p_25 = 0; p_25 < 9; p_25 += 1)
    if (p_25)
      goto lbl_29;
  unsigned short l_53;
  for (0; l_53; l_53 = (unsigned short) foo)
    {
    }
  return 0;
}
