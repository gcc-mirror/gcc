/* Test Attribute Vector associated with vector type stabs.  */
/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target stabs } */
/* { dg-options "-gstabs+ -fno-eliminate-unused-debug-types -faltivec" } */

int main ()
{
  vector int vi = { 6,7,8,9 };
  return 0;
}

/* { dg-final { scan-assembler ".stabs.*vi\:\\(0,\[0-9\]+\\)=\@V" } } */
