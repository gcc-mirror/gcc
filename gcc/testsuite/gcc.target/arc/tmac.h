/* Test MAC operations.  */

long long foo (long long a, int b, int c)
{
  a += (long long )c * (long long )b;
  return a;
}

long long foo1 (int b, int c)
{
  return (long long)c * (long long) b;
}

long long foo3 (long long a, unsigned int b, unsigned int c)
{
  a += (unsigned long long )c * (unsigned long long )b;
  return a;
}

long long foo4 (unsigned int b, unsigned int c)
{
  return (unsigned long long)c * (unsigned long long) b;
}

