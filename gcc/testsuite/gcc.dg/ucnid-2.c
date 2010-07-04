/* { dg-do run } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */
/* { dg-skip-if "" { ! ucn } { "*" } { "" } } */
/* { dg-options "-std=c99 -fextended-identifiers" } */
void abort (void);

static int \u00C0 = 1;
static int \u00C1 = 2;
static int \U000000C2 = 3;
static int wh\u00ff = 4;
static int a\u00c4b\u0441\U000003b4e = 5;

int main (void)
{
  
  if (\u00C0 != 1)
    abort ();
  if (\u00c1 != 2)
    abort ();
  if (\u00C2 != 3)
    abort ();
  if (wh\u00ff != 4)
    abort ();
  if (a\u00c4b\u0441\U000003b4e != 5)
    abort ();
  
  return 0;
}
