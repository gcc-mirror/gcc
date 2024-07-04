/* { dg-do compile } */
/* { dg-add-options vect_early_break } */

int g_66, g_80_2;
void func_1func_41(int p_43)
{
lbl_1434:
  g_80_2 = 0;
  for (; g_80_2 <= 7; g_80_2 += 1) {
    g_66 = 0;
    for (; g_66 <= 7; g_66 += 1)
      if (p_43)
        goto lbl_1434;
  }
}
