static char * __attribute__((noinline))
test_1_callee (int flag, char *a, char *b)
{
  char *p;
  if (flag)
    p = a;
  else
    p = b;
  return p;
}

char test_1_caller(int flag) {
  char a = 42;
  char b = 43;
  return *test_1_callee(flag, &a, &b);
}
