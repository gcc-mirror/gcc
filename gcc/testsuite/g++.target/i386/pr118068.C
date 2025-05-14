// PR target/118068
// { dg-do compile { target c++20 } }
// { dg-options "-O0 -mavx" }

typedef float V __attribute__((vector_size (32)));

consteval unsigned char
foo (int x)
{
  return x;
}

V
bar (V x, V y)
{
  return __builtin_ia32_blendps256 (x, y, (int) foo (0x23));
}
