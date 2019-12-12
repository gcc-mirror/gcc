/* { dg-do run } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */
/* { dg-skip-if "" { ! ucn } } */
/* { dg-options "-std=c99 -g" } */
void abort (void);

int À = 1;
int Á = 2;
int Â = 3;
int whÿ = 4;
int aÄbсδe = 5;

int main (void)
{
  
  if (À != 1)
    abort ();
  if (Á != 2)
    abort ();
  if (Â != 3)
    abort ();
  if (whÿ != 4)
    abort ();
  if (aÄbсδe != 5)
    abort ();
  
  return 0;
}
