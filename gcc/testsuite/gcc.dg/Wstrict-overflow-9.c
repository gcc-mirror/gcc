/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=2" } */

/* Source: Ian Lance Taylor.  */

int
foo (int i)
{
  return __builtin_abs (i) >= 0; /* { dg-warning "assuming signed overflow does not occur" "correct warning" } */
}
