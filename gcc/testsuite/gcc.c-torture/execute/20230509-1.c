int __attribute__((noipa)) f(unsigned a, int b)
{
  if (a < 0) __builtin_unreachable();
  if (a > 30) __builtin_unreachable();
  int t = a;
  if (b)  t = 100;
  else  if (a != 0)
    t = a ;
  else
    t = 1;
  return t;
}


int main(void)
{
  if (f(0, 0) != 1)
    __builtin_abort();
  if (f(1, 0) != 1)
    __builtin_abort();
  if (f(0, 1) != 100)
    __builtin_abort();
  if (f(1, 0) != 1)
    __builtin_abort();
  if (f(30, 0) != 30)
    __builtin_abort();
}

