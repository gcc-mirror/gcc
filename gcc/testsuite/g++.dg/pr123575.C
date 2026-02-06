// { dg-do compile }
// { dg-options "-O -Wno-psabi" }

typedef int __attribute__((vector_size(sizeof(int)*2))) v2i;
typedef long __attribute__((vector_size(sizeof(long)*2))) v2l;

v2l f(v2i a)
{
  v2l  t = __builtin_convertvector(a, v2l);
  t = t > 0 ? t : -t;
  return t;
}
