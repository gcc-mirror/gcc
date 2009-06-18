/* Test various operators on __fp16 and mixed __fp16/float operands.  */

#include <assert.h>

#define CHECK(e,r) assert ((e) == r)
#define CHECK2(e,r) (assert ((e) == r), temp = (e), assert (temp == r))
#define TEST(e) assert (e)
#define TESTNOT(e) assert (!(e))

volatile __fp16 h0 = 0.0;
volatile __fp16 h1 = 1.0;
volatile __fp16 h42 = 42.0;
volatile __fp16 hm2 = -2.0;
volatile __fp16 temp;

volatile float f0 = 0.0;
volatile float f1 = 1.0;
volatile float f42 = 42.0;
volatile float fm2 = -2.0;

int main (void)
{
  TEST (h1);
  TESTNOT (h0);
  TEST (!h0);
  TESTNOT (!h1);

  CHECK2 (-h1, -1.0);
  CHECK2 (+h1, 1.0);

  CHECK (h1++, 1.0);
  CHECK (h1, 2.0);
  CHECK (++h1, 3.0);
  CHECK (h1, 3.0);

  CHECK (--h1, 2.0);
  CHECK (h1, 2.0);
  CHECK (h1--, 2.0);
  CHECK (h1, 1.0);

  CHECK2 (h42 * hm2, -84.0);
  CHECK2 (h42 * (__fp16) -2.0, -84.0);
  CHECK2 (h42 * fm2, -84.0);
  CHECK2 (f42 * hm2, -84.0);

  CHECK2 (h42 / hm2, -21.0);
  CHECK2 (h42 / (__fp16) -2.0, -21.0);
  CHECK2 (h42 / fm2, -21.0);
  CHECK2 (f42 / hm2, -21.0);

  CHECK2 (hm2 + h42, 40.0);
  CHECK2 ((__fp16)-2.0 + h42, 40.0);
  CHECK2 (hm2 + f42, 40.0);
  CHECK2 (fm2 + h42, 40.0);

  CHECK2 (hm2 - h42, -44.0);
  CHECK2 ((__fp16)-2.0 - h42, -44.0);
  CHECK2 (hm2 - f42, -44.0);
  CHECK2 (fm2 - h42, -44.0);

  TEST (hm2 < h42);
  TEST (hm2 < (__fp16)42.0);
  TEST (hm2 < f42);
  TEST (fm2 < h42);

  TEST (h42 > hm2);
  TEST ((__fp16)42.0 > hm2);
  TEST (h42 > fm2);
  TEST (f42 > hm2);

  TEST (hm2 <= h42);
  TEST (hm2 <= (__fp16)42.0);
  TEST (hm2 <= f42);
  TEST (fm2 <= h42);

  TEST (h42 >= hm2);
  TEST (h42 >= (__fp16)-2.0);
  TEST (h42 >= fm2);
  TEST (f42 >= hm2);

  TESTNOT (h1 == hm2);
  TEST (h1 == h1);
  TEST (h1 == (__fp16)1.0);
  TEST (h1 == f1);
  TEST (f1 == h1);

  TEST (h1 != hm2);
  TESTNOT (h1 != h1);
  TESTNOT (h1 != (__fp16)1.0);
  TESTNOT (h1 != f1);
  TESTNOT (f1 != h1);

  CHECK2 ((h1 ? hm2 : h42), -2.0);
  CHECK2 ((h0 ? hm2 : h42), 42.0);

  CHECK (h0 = h42, 42.0);
  CHECK (h0, 42.0);
  CHECK (h0 = (__fp16)-2.0, -2.0);
  CHECK (h0, -2.0);
  CHECK (h0 = f0, 0.0);
  CHECK (h0, 0.0);

  CHECK (h0 += h1, 1.0);
  CHECK (h0, 1.0);
  CHECK (h0 += (__fp16)1.0, 2.0);
  CHECK (h0, 2.0);
  CHECK (h0 += fm2, 0.0);
  CHECK (h0, 0.0);

  CHECK (h0 -= h1, -1.0);
  CHECK (h0, -1.0);
  CHECK (h0 -= (__fp16)1.0, -2.0);
  CHECK (h0, -2.0);
  CHECK (h0 -= fm2, 0.0);
  CHECK (h0, 0.0);

  h0 = hm2;
  CHECK (h0 *= hm2, 4.0);
  CHECK (h0, 4.0);
  CHECK (h0 *= (__fp16)-2.0, -8.0);
  CHECK (h0, -8.0);
  CHECK (h0 *= fm2, 16.0);
  CHECK (h0, 16.0);

  CHECK (h0 /= hm2, -8.0);
  CHECK (h0, -8.0);
  CHECK (h0 /= (__fp16)-2.0, 4.0);
  CHECK (h0, 4.0);
  CHECK (h0 /= fm2, -2.0);
  CHECK (h0, -2.0);

  CHECK ((h0, h1), 1.0);

  return 0;
}
