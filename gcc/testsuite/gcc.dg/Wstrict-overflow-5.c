/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=3" } */

/* Source: Ian Lance Taylor.  Based on strict-overflow-3.c.  */

/* We can only simplify the conditional when using strict overflow
   semantics.  */

int
foo (int i, int j)
{
  return i + 100 < j + 1000; /* { dg-warning "assuming signed overflow does not occur" "correct warning" } */
}
