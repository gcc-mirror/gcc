#define NO_WARN_X86_INTRINSICS 1

static void sse4_2_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse4_2_test ();
}

int
main ()
{
  do_test ();

  return 0;
}
