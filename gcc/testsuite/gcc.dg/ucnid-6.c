/* { dg-do run } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } } */
/* { dg-skip-if "" { ! ucn } } */
/* { dg-options "-std=c99 -save-temps -g" } */
void abort (void);

int \u00C0(void) { return 1; }
int \u00C1(void) { return 2; }
int \U000000C2(void) { return 3; }
int wh\u00ff(void) { return 4; }
int a\u00c4b\u0441\U000003b4e(void) { return 5; }

int main (void)
{
  
  if (\u00C0() != 1)
    abort ();
  if (\u00c1() != 2)
    abort ();
  if (\u00C2() != 3)
    abort ();
  if (wh\u00ff() != 4)
    abort ();
  if (a\u00c4b\u0441\U000003b4e() != 5)
    abort ();
  
  return 0;
}
