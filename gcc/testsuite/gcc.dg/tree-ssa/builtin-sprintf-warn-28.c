/* Verify that -Wformat-overflow uses context-sensitive ranges even
   at -O0 to avoid both false positives and negatives.
   { dg-do compile }
   { dg-options "-O0 -Wall" } */

extern int sprintf (char*, const char*, ...);

extern char a[4];

void test_context_range (int c)
{
  int i = 998;

  /* Prior to the switch to Ranger in GCC 12 -Wformat-overflow triggers
     for both calls because EVRP doesn't expose the accurate range of
     the argument.  */
  if (c)
    sprintf (a, "%i", i + 2);   // { dg-warning "writing a terminating nul past the end of the destination" }
  else
    sprintf (a, "%i", i + 1);   // { dg-bogus "-Wformat-overflow" }
}
