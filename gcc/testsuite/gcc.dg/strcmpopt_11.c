/* Verify that strcmp doesn't make assumptions about the size of a weak
   symbol.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

/* An ordinary definition of A with more elements might be provided
   in another translation unit.  Even though that would be undefined
   (the type of the actual definition must be the same as the type
   of the weak declaration) this test verifies that GCC doesn't rely
   on the size of this A for optimization (as a matter of QoI).  */
__attribute__ ((weak)) char a[3];

int cmp_a3_x (void)
{
  return __builtin_strcmp (a, "1234567") == 0;
}
