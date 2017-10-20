/* PR target/82498 */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

#include <x86intrin.h>

volatile unsigned int a;
volatile unsigned long long b;
volatile int c;

int
main ()
{
  a = 0x12345678U;
  a = __rold (a, 0);
  if (a != 0x12345678U)
    __builtin_abort ();
  a = __rold (a, 32);
  if (a != 0x12345678U)
    __builtin_abort ();
  a = __rold (a, -32);
  if (a != 0x12345678U)
    __builtin_abort ();
  a = __rold (a, 37);
  if (a != 0x468acf02U)
    __builtin_abort ();
  a = __rold (a, -5);
  if (a != 0x12345678U)
    __builtin_abort ();
  a = __rord (a, 0);
  if (a != 0x12345678U)
    __builtin_abort ();
  a = __rord (a, 32);
  if (a != 0x12345678U)
    __builtin_abort ();
  a = __rord (a, -32);
  if (a != 0x12345678U)
    __builtin_abort ();
  a = __rord (a, -37);
  if (a != 0x468acf02U)
    __builtin_abort ();
  a = __rord (a, 5);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = 0;
  a = __rold (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = 32;
  a = __rold (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = -32;
  a = __rold (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = 37;
  a = __rold (a, c);
  if (a != 0x468acf02U)
    __builtin_abort ();
  c = -5;
  a = __rold (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = 0;
  a = __rord (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = 32;
  a = __rord (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = -32;
  a = __rord (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
  c = -37;
  a = __rord (a, c);
  if (a != 0x468acf02U)
    __builtin_abort ();
  c = 5;
  a = __rord (a, c);
  if (a != 0x12345678U)
    __builtin_abort ();
#ifdef __x86_64__
  b = 0x123456789abcdef1ULL;
  b = __rolq (b, 0);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  b = __rolq (b, 64);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  b = __rolq (b, -64);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  b = __rolq (b, 69);
  if (b != 0x468acf13579bde22ULL)
    __builtin_abort ();
  b = __rolq (b, -5);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  b = __rorq (b, 0);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  b = __rorq (b, 64);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  b = __rorq (b, -64);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  b = __rorq (b, -69);
  if (b != 0x468acf13579bde22ULL)
    __builtin_abort ();
  b = __rorq (b, 5);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = 0;
  b = __rolq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = 64;
  b = __rolq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = -64;
  b = __rolq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = 69;
  b = __rolq (b, c);
  if (b != 0x468acf13579bde22ULL)
    __builtin_abort ();
  c = -5;
  b = __rolq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = 0;
  b = __rorq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = 64;
  b = __rorq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = -64;
  b = __rorq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
  c = -69;
  b = __rorq (b, c);
  if (b != 0x468acf13579bde22ULL)
    __builtin_abort ();
  c = 5;
  b = __rorq (b, c);
  if (b != 0x123456789abcdef1ULL)
    __builtin_abort ();
#endif
  return 0;
}
