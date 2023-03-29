
static void test (void)
{
}

static void (*p)(void);

static void foo (void)
{
  p = test;
  p();
}
