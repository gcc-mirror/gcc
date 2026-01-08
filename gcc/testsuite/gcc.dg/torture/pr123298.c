/* { dg-do run } */

__attribute__((noipa))
int
func_1 (int g_258, int func_1_BS_COND_11, int g_64)
{
  int BS_VAR_1 = 10;
  unsigned char BS_VAR_5[2] = { 19, 28 };
  int LOCAL_CHECKSUM = 0;
  if (func_1_BS_COND_11)
    goto BS_LABEL_0;
  BS_VAR_1 = 0;
  while (g_64 <= 5)
    {
BS_LABEL_0:
      for (;;)
	{
	  LOCAL_CHECKSUM = BS_VAR_5[1];
	  if (g_258 != 0) break;
	  goto out;
	}
      BS_VAR_5[BS_VAR_1 < 5] = 0;
      g_258 = 0;
    }
out:
  return LOCAL_CHECKSUM;
}

int
main ()
{
  if (func_1 (50, 0, 0) != 0)
    __builtin_abort ();
  return 0;
}
