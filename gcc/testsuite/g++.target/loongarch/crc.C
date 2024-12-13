/* { dg-do run } */
/* { dg-options "-O2" } */

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __SIZE_TYPE__ size_t;

template <class T, uint32_t poly>
__attribute__ ((always_inline)) inline uint32_t
crc32_impl (const T *data, size_t len)
{
  uint32_t ret = 0xffffffffu;
  for (size_t k = 0; k < len; k++)
    {
      ret ^= data[k];
      for (int i = 0; i < 8 * sizeof (T); i++)
	if (ret & 1)
	  ret = (ret >> 1) ^ poly;
	else
	  ret >>= 1;
    }
  return ret;
}

template <class T, uint32_t poly>
__attribute__ ((noipa, optimize (0))) uint32_t
crc32_ref (const T *data, size_t len)
{
  return crc32_impl<T, poly> (data, len);
}

template <class T, uint32_t poly>
__attribute__ ((noipa)) uint32_t
crc32_opt (const T *data, size_t len)
{
  return crc32_impl<T, poly> (data, len);
}

template <class T, uint32_t poly>
__attribute__ ((noipa)) uint32_t
crc32_alt (const T *data, size_t len)
{
  uint32_t ret = 0xffffffffu;
  for (size_t k = 0; k < len; k++)
    {
      T x = data[k];
      for (int i = 0; i < 8 * sizeof (T); i++)
	{
	  if ((ret & 1) ^ (x & 1))
	    ret = (ret >> 1) ^ poly;
	  else
	    ret >>= 1;
	  x >>= 1;
	}
    }
  return ret;
}

union test_data_t
{
  uint8_t u8[1024];
  uint16_t u16[512];
  uint32_t u32[256];

  operator const uint8_t * () const { return u8; }
  operator const uint16_t * () const { return u16; }
  operator const uint32_t * () const { return u32; }

  constexpr
  test_data_t ()
      : u8{}
  {
  }
};

/* Generate test data at compile time with minstd_rand0 algorithm.  */
constexpr test_data_t
gen (uint64_t seed)
{
  uint64_t state = seed;
  test_data_t ret;
  for (int i = 0; i < sizeof (ret); i++)
    {
      state = state * 16807 % 2147483647;
      ret.u8[i] = (uint8_t)state;
    }
  return ret;
}

constexpr union test_data_t test_data = gen (0xdeadbeef);

void
assert_eq (uint32_t x, uint32_t y)
{
  if (x != y)
    __builtin_trap ();
}

template <class T, uint32_t poly>
void
test_crc32 ()
{
  constexpr size_t len = sizeof (test_data) / sizeof (T);
  uint32_t ref = crc32_ref<T, poly> (test_data, len);
  assert_eq (ref, crc32_opt<T, poly> (test_data, len));
  assert_eq (ref, crc32_alt<T, poly> (test_data, len));
}

int
main (void)
{
  test_crc32<uint32_t, 0xEDB88320u> ();
  test_crc32<uint16_t, 0xEDB88320u> ();
  test_crc32<uint8_t, 0xEDB88320u> ();
  test_crc32<uint32_t, 0x82F63B78u> ();
  test_crc32<uint16_t, 0x82F63B78u> ();
  test_crc32<uint8_t, 0x82F63B78u> ();
}
