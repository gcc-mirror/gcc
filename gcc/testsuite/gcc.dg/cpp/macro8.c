/* { dg-do compile } */
/* { dg-options -std=gnu89 } */

/* GCC 2.95.2 used to get the following variable argument macro
   expansions wrong.

   Source: Neil Booth, from PR 3852 with persmission.  31 Jul 2001.  */

#define TEST_WORSE(args...) (5, ## args)
#define TEST_BAD(foo, args...) (2, (foo), ## args)

extern void abort ();

static int add (int a, int b)
{
  return a + b;
}

int main ()
{
  /* Would expand to a single closing parenthesis, maybe because of
     the "no args requires space" brokenness.  */
  if (TEST_WORSE () != 5)
    abort ();
  /* The macro would expand to (0, (0) with a missing closing parenthesis.  */
  if (add TEST_BAD (5) != 7)
    abort ();
  return 0;
}
