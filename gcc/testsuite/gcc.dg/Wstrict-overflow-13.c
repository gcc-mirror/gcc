/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=2" } */

/* Source: Ian Lance Taylor.  Dual of no-strict-overflow-6.c.  */

/* VRP test.  This turns into an infinite loop (depending on what
   bigtime_test does), but at least we warn about it.  */

extern int bigtime_test (int);
int
foo ()
{
  int j;
  for (j = 1; 0 < j; j *= 2) /* { dg-warning "assuming signed overflow does not occur" "correct warning" } */
    if (! bigtime_test (j))
      return 1;
  return 0;
}
