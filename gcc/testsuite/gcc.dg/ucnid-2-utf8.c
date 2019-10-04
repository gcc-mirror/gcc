/* { dg-do run } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */
/* { dg-skip-if "" { ! ucn } } */
/* { dg-options "-std=c99 -g" } */
void abort (void);

static int À = 1;
static int Á = 2;
static int Â = 3;
static int whÿ = 4;
static int aÄbсδe = 5;

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
