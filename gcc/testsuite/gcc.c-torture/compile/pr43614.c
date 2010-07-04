volatile int g_2[7];

void foo (unsigned);

int main (void)
{
  int i_459 = 0;
  int t2818;
  int t2819;
  volatile char *t2820;
  int t2821;
  volatile char *t2822;
  int *t2823;
  unsigned t2824;
LL655:
  t2822 = (volatile char *)g_2;
  t2821 = i_459;
  t2820 = t2822 + t2821;
  t2823 = (int *)t2820;
  t2824 = *t2823;
  foo (t2824);
  t2818 = i_459;
  t2819 = t2818 + 1;
  i_459 = t2819;
  goto LL655;
}

