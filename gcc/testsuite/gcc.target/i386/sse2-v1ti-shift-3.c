/* PR target/102986 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

typedef unsigned __int128 uv1ti __attribute__ ((__vector_size__ (16)));
typedef __int128 sv1ti __attribute__ ((__vector_size__ (16)));
typedef __int128 v1ti __attribute__ ((__vector_size__ (16)));

typedef unsigned __int128 uti;
typedef __int128 sti;
typedef __int128 ti;

uv1ti ashl_v1ti(uv1ti x, unsigned int i) { return x << i; }
uv1ti lshr_v1ti(uv1ti x, unsigned int i) { return x >> i; }
sv1ti ashr_v1ti(sv1ti x, unsigned int i) { return x >> i; }
uv1ti rotr_v1ti(uv1ti x, unsigned int i) { return (x >> i) | (x << (-i&127)); }
uv1ti rotl_v1ti(uv1ti x, unsigned int i) { return (x << i) | (x >> (-i&127)); }

uti ashl_ti(uti x, unsigned int i) { return x << i; }
uti lshr_ti(uti x, unsigned int i) { return x >> i; }
sti ashr_ti(sti x, unsigned int i) { return x >> i; }
uti rotr_ti(uti x, unsigned int i) { return (x >> i) | (x << (-i&127)); }
uti rotl_ti(uti x, unsigned int i) { return (x << i) | (x >> (-i&127)); }

void test(ti x)
{
  unsigned int i;
  uv1ti ut = (uv1ti)x;
  sv1ti st = (sv1ti)x;

  for (i=0; i<128; i++) {
    if ((ti)ashl_v1ti(ut,i) != (ti)ashl_ti(x,i))
      __builtin_abort();
    if ((ti)lshr_v1ti(ut,i) != (ti)lshr_ti(x,i))
      __builtin_abort();
    if ((ti)ashr_v1ti(st,i) != (ti)ashr_ti(x,i))
      __builtin_abort();
    if ((ti)rotr_v1ti(ut,i) != (ti)rotr_ti(x,i))
      __builtin_abort();
    if ((ti)rotl_v1ti(ut,i) != (ti)rotl_ti(x,i))
      __builtin_abort();
  }
}

int main()
{
  ti x;

  x = ((ti)0x0011223344556677ull)<<64 | 0x8899aabbccddeeffull;
  test(x);
  x = ((ti)0xffeeddccbbaa9988ull)<<64 | 0x7766554433221100ull;
  test(x);
  x = ((ti)0x0123456789abcdefull)<<64 | 0x0123456789abcdefull;
  test(x);
  x = ((ti)0xfedcba9876543210ull)<<64 | 0xfedcba9876543210ull;
  test(x);
  x = ((ti)0x0123456789abcdefull)<<64 | 0xfedcba9876543210ull;
  test(x);
  x = ((ti)0xfedcba9876543210ull)<<64 | 0x0123456789abcdefull;
  test(x);
  x = 0;
  test(x);
  x = 0xffffffffffffffffull;
  test(x);
  x = ((ti)0xffffffffffffffffull)<<64;
  test(x);
  x = ((ti)0xffffffffffffffffull)<<64 | 0xffffffffffffffffull;
  test(x);
  x = ((ti)0x5a5a5a5a5a5a5a5aull)<<64 | 0x5a5a5a5a5a5a5a5aull;
  test(x);
  x = ((ti)0xa5a5a5a5a5a5a5a5ull)<<64 | 0xa5a5a5a5a5a5a5a5ull;
  test(x);
  x = 0xffull;
  test(x);
  x = 0xff00ull;
  test(x);
  x = 0xff0000ull;
  test(x);
  x = 0xff000000ull;
  test(x);
  x = 0xff00000000ull;
  test(x);
  x = 0xff0000000000ull;
  test(x);
  x = 0xff000000000000ull;
  test(x);
  x = 0xff00000000000000ull;
  test(x);
  x = ((ti)0xffull)<<64;
  test(x);
  x = ((ti)0xff00ull)<<64;
  test(x);
  x = ((ti)0xff0000ull)<<64;
  test(x);
  x = ((ti)0xff000000ull)<<64;
  test(x);
  x = ((ti)0xff00000000ull)<<64;
  test(x);
  x = ((ti)0xff0000000000ull)<<64;
  test(x);
  x = ((ti)0xff000000000000ull)<<64;
  test(x);
  x = ((ti)0xff00000000000000ull)<<64;
  test(x);
  x = 0xdeadbeefcafebabeull;
  test(x);
  x = ((ti)0xdeadbeefcafebabeull)<<64;
  test(x);

  return 0;
}

