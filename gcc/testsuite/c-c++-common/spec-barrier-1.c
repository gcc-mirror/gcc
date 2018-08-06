/* { dg-do run } */
/* { dg-options "-O" } */

/* Test that __builtin_speculation_safe_value returns the correct value.  */
/* This test will cause an unfiltered warning to be emitted on targets
   that have not implemented support for speculative execution
   barriers.  They should fix that rather than disabling this
   test.  */
char a = 1;
short b = 2;
int c = 3;
long d = 4;
long long e = 5;
int *f = (int*) &c;
#ifdef __SIZEOF_INT128__
__int128 g = 9;
#endif

int main ()
{
  if (__builtin_speculation_safe_value (a) != 1)
    __builtin_abort ();
  if (__builtin_speculation_safe_value (b) != 2)
    __builtin_abort ();
  if (__builtin_speculation_safe_value (c) != 3)
    __builtin_abort ();
  if (__builtin_speculation_safe_value (d) != 4)
    __builtin_abort ();
  if (__builtin_speculation_safe_value (e) != 5)
    __builtin_abort ();
  if (__builtin_speculation_safe_value (f) != &c)
    __builtin_abort ();
#ifdef __SIZEOF_INT128__
  if (__builtin_speculation_safe_value (g) != 9)
    __builtin_abort ();
#endif
  return 0;
}
