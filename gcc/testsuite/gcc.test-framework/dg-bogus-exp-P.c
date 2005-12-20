/* Test the tester; previously part of gcc.misc-tests/dg-9.c.  */
/* { dg-prms-id 42 } */
/* { dg-options "-Wall" } */

int main (int argc, char *argv[])
{
  return 0; /* { dg-bogus "foobar" "bogus fail test" } */
}
