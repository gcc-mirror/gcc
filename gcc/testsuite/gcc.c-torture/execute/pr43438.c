extern void abort (void);

static unsigned char g_2 = 1;
static int g_9;
static int *l_8 = &g_9;

static void func_12(int p_13)
{
  int * l_17 = &g_9;
  *l_17 &= 0 < p_13;
}

int main(void)
{
  unsigned char l_11 = 254;
  *l_8 |= g_2;
  l_11 |= *l_8;
  func_12(l_11);
  if (g_9 != 1)
    abort ();
  return 0;
} 

