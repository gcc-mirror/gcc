/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=3" } */

/* Source: Ian Lance Taylor.  Based on strict-overflow-6.c.  */

/* We can only simplify the conditional when using strict overflow
   semantics.  */

int
foo (char* p)
{
  return p + 1000 < p; /* { dg-warning "assuming pointer wraparound does not occur" "correct warning" } */
}
