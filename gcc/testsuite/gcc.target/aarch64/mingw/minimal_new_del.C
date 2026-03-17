/* { dg-do run } */
/* { dg-additional-options "-O2" } */

/* A minimal test to validate that C++ support works.  */

int
main()
{
  int *x = new int;
  delete x;
  return 0;
}
