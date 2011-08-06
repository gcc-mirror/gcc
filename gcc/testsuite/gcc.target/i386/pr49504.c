/* PR target/49504 */
/* { dg-do run { target { x32 } } } */
/* { dg-options "-O" } */

unsigned long long 
foo (const void* p, unsigned long long q)
{
  unsigned long long a = (((unsigned long long) ((unsigned long) p)) + q) >> 32;
  return a;
}

int
main ()
{
  if (foo (foo, 0x100000000ULL) != 0x1)
    __builtin_abort ();
  return 0;
}
