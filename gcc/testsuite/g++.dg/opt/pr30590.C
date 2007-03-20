/* { dg-do run } */
/* { dg-options "-O" } */
struct test
{
  int type;
  char buffer[4242]; /* should trigger pass-by-reference */
};

int flag = 0;

struct test
reset (void)
{
  struct test retval;
  retval.type = 1;
  return retval;
}

struct test
test (void)
{
  struct test result;
  result.type = 0;

  for (int i = 0; i < 2; ++i)
    {
      struct test candidate = reset ();
      if (flag)
        result = candidate;
    }

  return result;
}

int
main (void)
{
  struct test result = test ();
  return result.type;
}
