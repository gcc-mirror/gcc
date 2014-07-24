/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */

struct test
  {
    long a __attribute__((aligned (16)));
  };

void test (struct test a) /* { dg-message "note: the ABI of passing aggregates with 16-byte alignment will change" } */
{
}

