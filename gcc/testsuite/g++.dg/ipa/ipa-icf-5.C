/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fdump-ipa-icf" } */

struct test
{
  int a;
  float b;
};

extern const struct test myarray __attribute__ ((visibility("hidden")));
extern const struct test myarray_alias __attribute__ ((visibility("hidden")));

const struct test myarray = {1, 1.5f};

extern const struct test myarray_alias __attribute__ ((alias ("myarray")));

int main()
{
  return myarray.a - myarray_alias.a;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
