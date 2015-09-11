/* PR c/63862 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

unsigned long long int __attribute__ ((noinline, noclone))
foo (unsigned long long int i, unsigned long long int j)
{
  asm ("");
  return i >> j;
}

unsigned long long int __attribute__ ((noinline, noclone))
bar (unsigned long long int i, unsigned long long int j)
{
  asm ("");
  return i << j;
}

int
main ()
{
  foo (1ULL, 0x100000000ULL);
  bar (1ULL, 0x100000000ULL);
}

/* { dg-output "shift exponent 4294967296 is too large for \[^\n\r]*-bit type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent 4294967296 is too large for \[^\n\r]*-bit type 'long long unsigned int'" } */
