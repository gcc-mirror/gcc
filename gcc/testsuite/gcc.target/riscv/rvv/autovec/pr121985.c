/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl512b -mabi=lp64d -mrvv-max-lmul=m8" } */

unsigned short a=3;
char f=1;

int main()
{
  for (char var=f; var<6; var++)
    a *= 5;

  return a;
}

/* We would set a wrong niter range that would cause us to extract the wrong
   element.  */
/* { dg-final { scan-assembler-not "vslidedown.vi.*,0" } } */
