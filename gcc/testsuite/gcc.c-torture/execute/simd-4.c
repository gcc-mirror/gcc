typedef int __attribute__((vector_size(8))) v2si;
long long s64;

static inline long long
__ev_convert_s64 (v2si a)
{
  return (long long) a;
}

int main()
{
  union { long long ll; int i[2]; } endianness_test;
  endianness_test.ll = 1;
  int little_endian = endianness_test.i[0];
  s64 = __ev_convert_s64 ((v2si){1,0xffffffff});
  if (s64 != (little_endian ? 0xffffffff00000001LL : 0x1ffffffffLL))
    abort ();
  return 0;
}
