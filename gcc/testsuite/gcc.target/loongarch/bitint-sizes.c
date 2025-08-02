/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23" } */

static long unsigned int
calc_size (int n)
{
  if (n > 128)
    return ((n - 1)/128 + 1)  * sizeof(__int128_t);
  if (n > 64)
    return sizeof(__int128_t);
  if (n > 32)
    return sizeof(long long);
  if (n > 16)
    return sizeof(int);
  if (n > 8)
    return sizeof(short);
  else
    return sizeof(char);
}

#define CHECK_SIZE(N) \
  if (sizeof(_BitInt(N)) != calc_size(N)) \
    __builtin_abort ();

int main (void)
{
  CHECK_SIZE(2);
  CHECK_SIZE(3);
  CHECK_SIZE(7);
  CHECK_SIZE(8);
  CHECK_SIZE(9);
  CHECK_SIZE(13);
  CHECK_SIZE(15);
  CHECK_SIZE(16);
  CHECK_SIZE(17);
  CHECK_SIZE(24);
  CHECK_SIZE(31);
  CHECK_SIZE(32);
  CHECK_SIZE(33);
  CHECK_SIZE(42);
  CHECK_SIZE(53);
  CHECK_SIZE(63);
  CHECK_SIZE(64);
  CHECK_SIZE(65);
  CHECK_SIZE(79);
  CHECK_SIZE(96);
  CHECK_SIZE(113);
  CHECK_SIZE(127);
  CHECK_SIZE(128);
  CHECK_SIZE(129);
  CHECK_SIZE(153);
  CHECK_SIZE(255);
  CHECK_SIZE(256);
  CHECK_SIZE(257);
  CHECK_SIZE(353);
  CHECK_SIZE(512);
  CHECK_SIZE(620);
  CHECK_SIZE(1024);
  CHECK_SIZE(30000);
}
