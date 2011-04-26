/* { dg-do compile } */

extern volatile int g_89[5][9];
extern int g, *gp;
void func_64()
{
  int i;
  for (i = 0; i < 1; )
    {
      for (g = 0; g < 1; )
	return;
      gp = (int *)&g_89[g][0];
    }
}
