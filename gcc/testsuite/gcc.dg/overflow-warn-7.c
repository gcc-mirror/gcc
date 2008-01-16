/* { dg-do compile } */
/* { dg-options "-Wall" } */

int f(char *device)
{
  return device == ((char *)0 + ~0UL);  /* { dg-bogus "overflow" } */
}

