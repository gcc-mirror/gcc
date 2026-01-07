/* { dg-do run } */

int main()
{
  int g_58;
  _Bool g_170 = 0;
  int m = 0;
  for (int i = 0; i < 2; i++)
    {
      int arr[3] = {};
label:
      g_58 = g_170 == 0;
      for (int a = 0; a < 3; a++)
	m += arr[a];
      for (int j = 0; j != 7; ++j)
	for (int k = 0; k < 2; k++)
	  {
	    --g_170;
	    if (g_58) goto label;
	    arr[0] = 2;
	  }
    }
  if (m != 0)
    __builtin_abort();
  return 0;
}
