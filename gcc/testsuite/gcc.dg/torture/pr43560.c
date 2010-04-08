/* { dg-do run } */
/* { dg-require-weak "" } */

int g_6[1][2] = {{1,1}};
int g_34 = 0;
int *const g_82 = &g_6[0][1];
int *g_85[2][1] __attribute__((weak));

void __attribute__((noinline))
func_4 (int x)
{
  int i;
  for (i = 0; i <= x; i++) {
      if (g_6[0][1]) {
	  *g_82 = 1;
      } else {
	  int **l_109 = &g_85[1][0];
	  if (&g_82 != l_109) {
	  } else {
	      *l_109 = &g_6[0][1];
	  }
	  *g_82 = 1;
      }
  }
}

int main (void)
{
  g_85[0][0] = &g_34;
  g_85[1][0] = &g_34;
  func_4(1);
  return 0;
} 

