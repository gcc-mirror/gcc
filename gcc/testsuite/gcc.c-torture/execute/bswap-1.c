/* Test __builtin_bswap64 . */

unsigned long long g(unsigned long long a) __attribute__((noinline));
unsigned long long g(unsigned long long a)
{
  return __builtin_bswap64(a);
}


unsigned long long f(unsigned long long c)
{
  union {
    unsigned long long a;
    unsigned char b[8];
  } a, b;
  a.a = c;
  b.b[0] = a.b[7];
  b.b[1] = a.b[6];
  b.b[2] = a.b[5];
  b.b[3] = a.b[4];
  b.b[4] = a.b[3];
  b.b[5] = a.b[2];
  b.b[6] = a.b[1];
  b.b[7] = a.b[0];
  return b.a;
}

int main(void)
{
  unsigned long long i;
  /* The rest of the testcase assumes 8 byte long long. */
  if (sizeof(i) != sizeof(char)*8)
    return 0;
  if (f(0x12) != g(0x12))
    __builtin_abort();
  if (f(0x1234) != g(0x1234))
    __builtin_abort();
  if (f(0x123456) != g(0x123456))
    __builtin_abort();
  if (f(0x12345678ull) != g(0x12345678ull))
    __builtin_abort();
  if (f(0x1234567890ull) != g(0x1234567890ull))
    __builtin_abort();
  if (f(0x123456789012ull) != g(0x123456789012ull))
    __builtin_abort();
  if (f(0x12345678901234ull) != g(0x12345678901234ull))
    __builtin_abort();
  if (f(0x1234567890123456ull) != g(0x1234567890123456ull))
    __builtin_abort();
  return 0;
}
