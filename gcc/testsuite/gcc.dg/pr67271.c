/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-require-effective-target int32plus } */

extern long int labs (long int j);
int
main ()
{
  long *a = (long *)"empty";
  int i = 1441516387;
  a[i] = labs (a[i]);
  return 0;
}
