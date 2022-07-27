int rglobal = 0;

volatile int g;
volatile int c;
volatile double *array;

/* unused parameter */
static void
bar(int *p)
{
  int i;
  for (i = 0; i < c; i++)
    {
      /* something big so that it is inlined second. */
      array[i] = (array[i+1]+array[i]+1)*2;
    }
}

void foo(int *p) {
  g = *p;
  bar(p);
}

void __attribute__((noinline))
entry(void)
{
  foo(&rglobal);
}

void __attribute__((used))
blah(int *p)
{
  bar(p);
}

