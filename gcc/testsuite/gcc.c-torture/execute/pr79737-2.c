/* PR tree-optimization/79737 */

#if __SIZEOF_INT__ < 4
  __extension__ typedef __INT32_TYPE__ int32_t;
#else
  typedef int int32_t;
#endif

#pragma pack(1)
struct S
{
  int32_t b:18;
  int32_t c:1;
  int32_t d:24;
  int32_t e:15;
  int32_t f:14;
} i, j;

void
foo ()
{
  i.e = 0;
  i.b = 5;
  i.c = 0;
  i.d = -5;
  i.f = 5;
}

void
bar ()
{
  j.b = 5;
  j.c = 0;
  j.d = -5;
  j.e = 0;
  j.f = 5;
}

int
main ()
{
  foo ();
  bar ();
  asm volatile ("" : : : "memory");
  if (i.b != j.b || i.c != j.c || i.d != j.d || i.e != j.e || i.f != j.f)
    __builtin_abort ();
}
