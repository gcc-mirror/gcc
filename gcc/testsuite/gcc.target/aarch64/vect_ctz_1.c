/* { dg-do run } */
/* { dg-options "-O3 -save-temps -fno-inline -fno-vect-cost-model" } */

extern void abort ();

#define TEST(name, subname, count) \
void \
count_tz_##name (unsigned *__restrict a, int *__restrict b) \
{ \
  int i; \
  for (i = 0; i < count; i++) \
    b[i] = __builtin_##subname##g (a[i], 32); \
}

#define CHECK(name, count, input, output) \
  count_tz_##name (input, output); \
  for (i = 0; i < count; i++) \
    { \
      if (output[i] != r[i]) \
	abort (); \
    }

TEST (v4si, ctz, 4)
TEST (v2si, ctz, 2)
/* { dg-final { scan-assembler "clz\tv\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "clz\tv\[0-9\]+\.2s" } } */

int
main ()
{
  unsigned int x4[4] = { 0x0, 0xFF80, 0x1FFFF, 0xFF000000 };
  int r[4] = { 32, 7, 0, 24 };
  int d[4], i;

  CHECK (v4si, 4, x4, d);
  CHECK (v2si, 2, x4, d);

  return 0;
}

