/* { dg-do compile } */
/* { dg-options "-w" } */

static unsigned short
foo (unsigned short ui1, unsigned short ui2)
{
  return ui1 - ui2;
}

static unsigned short
bar (unsigned ui1, unsigned short ui2)
{
  return ui1 + ui2;
}

struct S1
{
  const short f3;
};
int g_31;
short g_67;
struct S1 g_68[2][5][9][1][1] = {
};

int func_90 (int);

int int329 (int * const *const int32p_81, short ** p_82)
{
  short l_169[8];
  for (g_31 = 0; g_31 <= 0; g_31 = foo (g_31, 1))
    {
      short l_85;
lbl_89:g_67 ^= l_85;
       for (l_85 = 0; l_85 >= 0; l_85 = (short) bar)
	 if (g_31)
	   goto lbl_89;
       func_90 (1), g_68[0][2][2][0][0].f3, 0;
    }
  return l_169[6];
}

