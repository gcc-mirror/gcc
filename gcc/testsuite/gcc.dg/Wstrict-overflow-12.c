/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=2" } */

/* Source: Ian Lance Taylor.  Dual of no-strict-overflow-6.c.  */

/* VRP test.  This turns into an infinite loop when using strict
   overflow semantics.  */

int
foo ()
{
  int i, bits;
  for (i = 1, bits = 1; i > 0; i += i) /* { dg-warning "assuming signed overflow does not occur" "correct warning" { xfail *-*-* } } */
    ++bits;
  return bits;
}
