/* Test invalid bit-field types: bug 18498.  */
/* { dg-do compile } */
/* { dg-options "" } */

int
main(void)
{
  struct X {
    int s[20] : 1; /* { dg-error "error: bit-field 's' has invalid type" } */
    int *p : 2; /* { dg-error "error: bit-field 'p' has invalid type" } */
    int (*f)(float) : 3; /* { dg-error "error: bit-field 'f' has invalid type" } */
  } x;
  return 0;
}
