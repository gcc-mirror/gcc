// { dg-additional-options "-fopenmp-simd" }
// { dg-additional-options "-mavx2" { target { avx2_runtime } } }

extern "C" int memcmp(const void *s1, const void *s2, __SIZE_TYPE__ n);
extern "C" void abort(void);

template <typename T>
T reverseBits(T x)
{
  unsigned int s = sizeof(x) * 8;
  T mask = ~T(0);
  while ((s >>= 1) > 0)
    {
      mask ^= (mask << s);
      x = ((x >> s) & mask) | ((x << s) & ~mask); // unsupported use in stmt
    }
  return x;
}

void __attribute__((noinline,noipa))
test_reverseBits(unsigned* x)
{
#pragma omp simd aligned(x:32)
  for (int i = 0; i < 16; ++i)
    x[i] = reverseBits(x[i]); // couldn't vectorize loop
}

int main()
{
  unsigned arr[16] __attribute__((aligned(32)))
    = { 0x01020304, 0x05060708, 0x0a0b0c0d, 0x0e0f1011,
        0x11121314, 0x45065708, 0xfa0b3c0du, 0x0e0f1211,
        0x21222324, 0x55066708, 0xfa0b2c0du, 0x1e0f1011,
        0x31323334, 0x65067708, 0xfa0b5c0du, 0x0e3f1011 };
  unsigned arr2[16]
    = { 0x20c04080, 0x10e060a0, 0xb030d050, 0x8808f070u,
        0x28c84888, 0x10ea60a2, 0xb03cd05f, 0x8848f070u,
        0x24c44484, 0x10e660aa, 0xb034d05f, 0x8808f078u, 
        0x2ccc4c8c, 0x10ee60a6, 0xb03ad05f, 0x8808fc70u };

  test_reverseBits (arr);

  if (memcmp (arr, arr2, sizeof (arr)) != 0)
    abort ();
  return 0;
}

// { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" { target { vect_var_shift && vect_int } } } }
