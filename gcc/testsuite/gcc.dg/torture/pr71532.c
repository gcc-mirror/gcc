/* PR rtl-optimization/71532 */
/* { dg-do run } */
/* { dg-additional-options "-mtune=slm" { target i?86-*-* x86_64-*-* } } */

__attribute__((noinline, noclone, pure)) int
foo (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l)
{
  a++; b++; c++; d++; e++; f++; g++; h++; i++; j++; k++; l++;
  asm volatile ("" : : "g" (&a), "g" (&b), "g" (&c), "g" (&d) : "memory");
  asm volatile ("" : : "g" (&e), "g" (&f), "g" (&g), "g" (&h) : "memory");
  asm volatile ("" : : "g" (&i), "g" (&j), "g" (&k), "g" (&l) : "memory");
  return a + b + c + d + e + f + g + h + i + j + k + l;
}

__attribute__((noinline, noclone, pure)) int
bar (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l)
{
  a++; b++; c++; d++; e++; f++; g++; h++; i++; j++; k++; l++;
  asm volatile ("" : : "g" (&a), "g" (&b), "g" (&c), "g" (&d) : "memory");
  asm volatile ("" : : "g" (&e), "g" (&f), "g" (&g), "g" (&h) : "memory");
  asm volatile ("" : : "g" (&i), "g" (&j), "g" (&k), "g" (&l) : "memory");
  return 2 * a + b + c + d + e + f + g + h + i + j + k + l;
}

__attribute__((noinline, noclone)) int
test ()
{
  int a = foo (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
  a += bar (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
  return a;
}

int
main ()
{
  if (test () != 182)
    __builtin_abort ();
  return 0;
}
