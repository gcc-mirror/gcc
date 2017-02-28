/* PR tree-optimization/79737 */

#pragma pack(1)
struct S
{
  int b:18;
  int c:1;
  int d:24;
  int e:15;
  int f:14;
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
