extern int bar(void);

short s;

int foo(void)
{
  s = bar();
  return s;
}

