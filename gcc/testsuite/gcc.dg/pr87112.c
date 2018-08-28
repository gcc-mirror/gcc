/* PR tree-optimization/87112 - ICE due to strnlen mixing integer types
   { dg-do compile }
   { dg-options "-Os -Wall" } */

typedef __SIZE_TYPE__ size_t;

extern size_t strnlen (const char*, size_t);

size_t fi (int i)
{
  int n = i & 3;
  return strnlen ("int", n);
}

size_t fui (unsigned i)
{
  unsigned n = i & 3;
  return strnlen ("unsigned", n);
}

size_t fl (long i)
{
  long n = i & 3;
  return strnlen ("long", n);
}

size_t fsz (size_t i)
{
  size_t n = i & 3;
  return strnlen ("size_t", n);
}
