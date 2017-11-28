// PR c++/82414
// { dg-lto-do link }
// { dg-lto-options { { -flto -g } } }

typedef __attribute__ ((__aligned__ (16))) struct S { __extension__ unsigned long long Part[2]; } T; // bogus warning "violates one definition rule"

int
main ()
{
  T tf;
  asm volatile ("" : : "g" (__alignof__(tf)), "g" (__alignof__ (struct S)), "g" (__alignof__ (T)));
  return 0;
}
