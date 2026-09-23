/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-cp" } */

void
a ()
{
#pragma omp parallel sections
  {
    ;
  }
}
