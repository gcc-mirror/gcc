// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target sse2_runtime }
// { dg-options "-O -msse2" }

extern "C" void abort (void);

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
typedef short A __attribute__((__may_alias__));

__m128i __attribute__((noinline))
shuf(const __m128i v)
{
  __m128i r;

  reinterpret_cast<A *>(&r)[5] = reinterpret_cast<const A *>(&v)[4];
  return r;
}

int main()
{
  __attribute__((aligned(16))) short mem[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };

  *reinterpret_cast<__m128i *>(mem) = shuf (*reinterpret_cast<__m128i *>(mem));

  if (mem[5] != 4)
    abort ();

  return 0;
}
