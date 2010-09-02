int g_19;
int *g_42;
int **volatile g = &g_42;
int g_67[5][9][2][1] = {
};

int
func_4 (int p_5, unsigned char p_6, unsigned char p_7)
{
  unsigned char l_8[1];
  if (p_6)
    goto lbl_13;
  for (p_6 = 0; p_6; p_6 = (p_6, 0))
    if (0)
      {
      }
    else
      lbl_13:for (p_6 = 0; p_6 < 1; p_6 += 1)
	  l_8[p_6] = 0;
  return 0;
}

int *
func_45 (unsigned long p_46, unsigned char p_47)
{
  int *l_56 = &g_19;
  (void *)&l_56 != (void *)&g | !1 == func_4 (0, g_67[2][6][1][0], 0) ^ func_4 (1, 0, 0);
  return 0;
}

