/* { dg-do compile } */
/* { dg-set-compiler-env-var SOURCE_DATE_EPOCH "AAA" } */

/* Make sure that SOURCE_DATE_EPOCH is only parsed once */

int
main(void)
{
  __builtin_printf ("%s %s\n", __DATE__, __TIME__); /* { dg-error "SOURCE_DATE_EPOCH must expand" } */
  __builtin_printf ("%s %s\n", __DATE__, __TIME__);
  return 0;
}
