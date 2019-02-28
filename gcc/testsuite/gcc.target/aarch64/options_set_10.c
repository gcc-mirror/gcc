/* { dg-do compile { target "aarch64*-*-linux*" } } */
/* { dg-additional-options "-mcpu=native" } */

int main ()
{
  return 0;
}

/* { dg-final { scan-assembler-not {\.arch .+\+profile.*} } } */

 /* Check that an empty feature string is not detected during mcpu=native.  */
