struct clock {
  long sec; long usec;
};
        
int foo(void)
{
  struct clock clock_old = {0, 0};

  for (;;) {
    long foo;

    if (foo == clock_old.sec && 0 == clock_old.usec);
  }
  return 0;
}
                                        
