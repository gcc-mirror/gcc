#if __SIZEOF_LONG_LONG__ == 8
typedef unsigned long long V __attribute__((vector_size(16), may_alias));

struct S
{
  V in, mask, out;
};

struct S tests[] = {
  {
    { 0x1111111111111111, 0x2222222222222222 },
    { 0, 1 },
    { 0x1111111111111111, 0x2222222222222222 },
  },
  {
    { 0x1111111111111111, 0x2222222222222222 },
    { 0x0102030405060700, 0xffeeddccbbaa99f1 },
    { 0x1111111111111111, 0x2222222222222222 },
  },
  {
    { 0x1111111111111111, 0x2222222222222222 },
    { 1, 0 },
    { 0x2222222222222222, 0x1111111111111111 },
  },
  {
    { 0x1111111111111111, 0x2222222222222222 },
    { 0, 0 },
    { 0x1111111111111111, 0x1111111111111111 },
  },
  {
    { 0x1122334455667788, 0x99aabbccddeeff00 },
    { 1, 1 },
    { 0x99aabbccddeeff00, 0x99aabbccddeeff00 },
  },
  {
    { 0x1122334455667788, 0x99aabbccddeeff00 },
    { 1, 0 },
    { 0x99aabbccddeeff00, 0x1122334455667788 },
  },
};

extern void abort(void);

int main()
{
  int i;

  for (i = 0; i < sizeof(tests)/sizeof(tests[0]); ++i)
    {
      V r = __builtin_shuffle(tests[i].in, tests[i].mask);
      if (__builtin_memcmp(&r, &tests[i].out, sizeof(V)) != 0)
	abort();
    }

  return 0;
}

#endif /* SIZEOF_LONG_LONG */
