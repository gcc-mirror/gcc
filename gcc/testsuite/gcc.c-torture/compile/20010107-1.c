unsigned long x[4];

void foo(void)
{
  ((void (*)())(x+2))();
}
