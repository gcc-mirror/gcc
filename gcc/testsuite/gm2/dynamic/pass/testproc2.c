
int testproc2_x;

void _M2_init (void)
{
  testproc2_x = 1;
}

int bar (void)
{
  int t, t1;
  int c1;

  t = testproc2_x;
  t1 = t + 1;
  testproc2_x = t1;
  c1 = testproc2_x;
  return c1;
}


