/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23" } */

static long unsigned int
calc_alignof (int n)
{
  if (n > 64)
    return alignof(__int128_t);
  if (n > 32)
    return alignof(long long);
  if (n > 16)
    return alignof(int);
  if (n > 8)
    return alignof(short);
  else
    return alignof(char);
}

#define CHECK_ALIGNMENT(N) \
  if (alignof(_BitInt(N)) != calc_alignof(N)) \
    __builtin_abort ();

int main (void)
{
  CHECK_ALIGNMENT(2);
  CHECK_ALIGNMENT(3);
  CHECK_ALIGNMENT(7);
  CHECK_ALIGNMENT(8);
  CHECK_ALIGNMENT(9);
  CHECK_ALIGNMENT(13);
  CHECK_ALIGNMENT(15);
  CHECK_ALIGNMENT(16);
  CHECK_ALIGNMENT(17);
  CHECK_ALIGNMENT(24);
  CHECK_ALIGNMENT(31);
  CHECK_ALIGNMENT(32);
  CHECK_ALIGNMENT(33);
  CHECK_ALIGNMENT(42);
  CHECK_ALIGNMENT(53);
  CHECK_ALIGNMENT(63);
  CHECK_ALIGNMENT(64);
  CHECK_ALIGNMENT(65);
  CHECK_ALIGNMENT(79);
  CHECK_ALIGNMENT(96);
  CHECK_ALIGNMENT(113);
  CHECK_ALIGNMENT(127);
  CHECK_ALIGNMENT(128);
  CHECK_ALIGNMENT(129);
  CHECK_ALIGNMENT(153);
  CHECK_ALIGNMENT(255);
  CHECK_ALIGNMENT(256);
  CHECK_ALIGNMENT(257);
  CHECK_ALIGNMENT(353);
  CHECK_ALIGNMENT(512);
  CHECK_ALIGNMENT(620);
  CHECK_ALIGNMENT(1024);
  CHECK_ALIGNMENT(30000);
}
