// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-options "-std=c++11 -Os" }

enum class X : unsigned char {
  V = 2,
};

static void
__attribute__((noinline,noclone))
foo(unsigned &out, unsigned a, X b)
{
  out = static_cast<unsigned>(b);
}

int main()
{
  unsigned deadbeef = 0xDEADBEEF;
  asm volatile ("" : "+d" (deadbeef), "+c" (deadbeef));

  unsigned out;
  foo(out, 2, X::V);

  if (out != 2)
    __builtin_abort ();

  return 0;
}
