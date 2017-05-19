/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O2" } */

int foo ()
{
  char buf[8];
  return *((int *)buf); /* { dg-bogus "strict-aliasing" } */
}

