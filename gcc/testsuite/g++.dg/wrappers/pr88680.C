// { dg-do compile { target c++11 } }
// { dg-options "-Wtype-limits -fno-short-enums" }

const unsigned N = 8;
const unsigned P = 0;

enum { FOO, BAR };

static_assert (N >= 0 && N % 2 == 0, "");
static_assert (FOO >= 0, "");
static_assert (FOO >= FOO, "");
static_assert (FOO >= P, "");
static_assert (BAR >= P, "");
static_assert (N >= FOO, "");

void test(unsigned n)
{
  if (N >= 0 && N % 2 == 0)
    return;
  if (FOO >= 0)
    return;
  if (FOO >= FOO)
    return;
  if (FOO >= P)
    return;
  if (BAR >= P)
    return;
  if (N >= FOO)
    return;
  if (n >= 0) // { dg-warning "'>= 0' is always true" }
    return;
  if (n < 0) // { dg-warning "'< 0' is always false" }
    return;
  if (n >= FOO)
    return;
  if (n < FOO)
    return;
  if (N >= 0)
    return;
  if (N < 0)
    return;
  if (N >= FOO)
    return;
  if (N < FOO)
    return;
  if (0 <= FOO)
    return;
  if (0 <= n) // { dg-warning "'>= 0' is always true" }
    return;
  if (0 > n) // { dg-warning "'< 0' is always false" }
    return;
  if (N <= FOO)
    return;
  if (N <= n)
    return;
}
