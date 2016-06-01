/* { dg-do run } */
/* { dg-set-compiler-env-var SOURCE_DATE_EPOCH "630333296" } */

int
main(void)
{
  __builtin_printf ("%s %s\n", __DATE__, __TIME__);
  return 0;
}

/* { dg-output "^Dec 22 1989 12:34:56\n$" } */
