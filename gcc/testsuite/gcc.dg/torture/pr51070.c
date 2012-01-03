/* { dg-do compile } */

int
func_4 (int si1, int si2)
{
  return si1;
}

int
func_14 (int left, int right)
{
  return 1;
}

int
func_37 (int left, int right)
{
  return left;
}

int g_92[1024];
int g_95[1024];
int g_224;
int g_352[1024];
int
func_9 ()
{
  for (; g_224; g_224 += 1)
    {
      g_95[0] = func_4 (func_37 (g_92[g_224], 0), 0);
      g_92[g_224] = 0, g_352[g_224] = func_14 (0, 0);
    }
  return 0;
}
