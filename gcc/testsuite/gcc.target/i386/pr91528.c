/* PR target/91528 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-Os -mavx512vbmi2 -mforce-drap" } */

extern long int labs (long int j);

int
main ()
{
  long *a = (long *)"empty";
  int i = 1441516387;
  a[i] = labs (a[i]);
  return 0;
}
