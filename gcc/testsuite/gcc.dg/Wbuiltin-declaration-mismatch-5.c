/* PR testsuite/88098 - FAIL: gcc.dg/Wbuiltin-declaration-mismatch-4.c
   { dg-do compile }
   { dg-options "-Wbuiltin-declaration-mismatch -fshort-enums" } */

int abs ();
double fabs ();     /* { dg-message "built-in .fabs. declared here" } */

enum E { e0 } e;

int i;
double d;

void test_short_enums (void)
{
  /* enum e promotes to int.  */
  i = abs (e);

  d = fabs (e);     /* { dg-warning ".fabs. argument 1 promotes to .int. where .double. is expected in a call to built-in function declared without prototype" } */
}
