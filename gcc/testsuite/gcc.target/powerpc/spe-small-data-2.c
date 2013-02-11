/* Verify that we don't ICE trying to put float data in .sdata2.  */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_eabi_ok } */
/* { dg-options "-msdata=eabi -mcall-eabi -G 8" } */

double x;

int main(void)
{
  x = x * 2;
  return(0);
}

