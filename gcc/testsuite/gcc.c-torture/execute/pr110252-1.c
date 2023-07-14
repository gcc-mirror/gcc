/* This is reduced from sel-sched.cc which was noticed was being miscompiled too. */
int g(int min_need_stall) __attribute__((__noipa__));
int g(int min_need_stall)
{
  return  min_need_stall < 0 ? 1 : ((min_need_stall) < (1) ? (min_need_stall) : (1));
}
int main(void)
{
  for(int i = -100; i <= 100; i++)
    {
      int t = g(i);
      if (t != (i!=0))
        __builtin_abort();
    }
}
