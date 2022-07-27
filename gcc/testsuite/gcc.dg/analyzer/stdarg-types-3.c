static void __attribute__((noinline))
__analyzer_consume_n_ints (int num, ...)
{
  __builtin_va_list ap;
  __builtin_va_start (ap, num);

  int i, v;
  for (i = 0; i < num; i++)
    v = __builtin_va_arg (ap, int);

  __builtin_va_end (ap);
}

void test_int (int x)
{
  __analyzer_consume_n_ints (1, x);
}

void test_3_ints (int x, int y, int z)
{
  __analyzer_consume_n_ints (3, x, y, z);
}

/* Verify that we don't complain about types that get promoted to int
   at the variadic call.  */

void test_short (short s)
{
  __analyzer_consume_n_ints (1, s);
}

void test_ushort (unsigned short s)
{
  __analyzer_consume_n_ints (1, s);
}

void test_schar (signed char ch)
{
  __analyzer_consume_n_ints (1, ch);
}

void test_uchar (unsigned char ch)
{
  __analyzer_consume_n_ints (1, ch);
}

struct ust
{
  int b0123 : 4;
  int b4567 : 4;
};

void test_signed_bitfield (struct ust s)
{
  __analyzer_consume_n_ints (2, s.b0123, s.b4567);
}

struct sst
{
  unsigned int b0123 : 4;
  unsigned int b4567 : 4;
};

void test_unsigned_bitfield (struct sst s)
{
  __analyzer_consume_n_ints (2, s.b0123, s.b4567);
}
