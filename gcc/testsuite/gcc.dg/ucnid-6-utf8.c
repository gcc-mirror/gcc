/* { dg-do run } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } } */
/* { dg-skip-if "" { ! ucn } } */
/* { dg-options "-std=c99 -save-temps -g" } */
void abort (void);

int À(void) { return 1; }
int Á(void) { return 2; }
int Â(void) { return 3; }
int whÿ(void) { return 4; }
int aÄbсδe(void) { return 5; }

int main (void)
{
  
  if (À() != 1)
    abort ();
  if (Á() != 2)
    abort ();
  if (Â() != 3)
    abort ();
  if (whÿ() != 4)
    abort ();
  if (aÄbсδe() != 5)
    abort ();
  
  return 0;
}
