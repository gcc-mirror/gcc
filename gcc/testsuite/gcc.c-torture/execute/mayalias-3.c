typedef struct __attribute__((__may_alias__)) { short x; } test;

test *p;

int g(int *a)
{
 p = (test*)a;
}

int f()
{
  int a;
  g(&a);
  a = 10;
  test s={1};
  *p=s;
  return a;
}

int main() {
  if (f() == 10)
    __builtin_abort();
  return 0;
}


