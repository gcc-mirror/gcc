/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=3" } */

/* Source: Ian Lance Taylor.  Based on strict-overflow-2.c.  */

/* We can only simplify the division when using strict overflow
   semantics.  */

int
foo (int i)
{
  return (i * 100) / 10;
}
