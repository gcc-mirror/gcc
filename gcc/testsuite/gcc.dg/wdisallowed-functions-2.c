/* { dg-do compile } */
/* { dg-options "-Wdisallowed-function-list=foo,foobar,bar,foobar" } */

int foobar (int i)
{
  return (i * 5);
}

int foobar1 (int i)
{
  return foobar (i);  /* { dg-warning "disallowed call to 'foobar'" } */
}
