/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wall -Wno-strict-overflow" } */

/* Source: Ian Lance Taylor.  */

int
foo (int i)
{
  return i + 10 > i;
}
