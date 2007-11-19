typedef struct __attribute__((__may_alias__)) { short x; } test;

int f() {
  int a=10;
  test *p=(test *)&a;
  p->x = 1;
  return a;
}

int main() {
  if (f() == 10)
    __builtin_abort();
  return 0;
}


