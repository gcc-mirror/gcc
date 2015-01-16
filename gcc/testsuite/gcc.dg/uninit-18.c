/* { dg-do compile }  */
/* { dg-options "-O -Wuninitialized" } */

char *foo(int bar, char *baz)
{
  char *tmp;

  if (bar & 3)
    tmp = baz;

  switch (bar) {
  case 1:
    tmp[5] = 7;    /* { dg-bogus "may be used uninitialized" } */
    break;
  case 2:
    tmp[11] = 15;  /* { dg-bogus "may be used uninitialized" } */
    break;
  default:
    tmp = 0;
    break;
  }

  return tmp;      /* { dg-bogus "may be used uninitialized" } */
}
