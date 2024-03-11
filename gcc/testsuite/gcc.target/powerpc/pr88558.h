long int test1 (double a)
{
  return __builtin_lrint (a);
}

long long test2 (double a)
{
  return __builtin_llrint (a);
}

int test3 (double a)
{
  return __builtin_irint (a);
}

long int test4 (float a)
{
  return __builtin_lrintf (a);
}

long long test5 (float a)
{
  return __builtin_llrintf (a);
}

int test6 (float a)
{
  return __builtin_irintf (a);
}
