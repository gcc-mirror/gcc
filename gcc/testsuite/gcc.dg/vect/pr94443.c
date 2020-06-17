/* { dg-do compile } */
/* { dg-additional-options "-march=znver2" { target { x86_64-*-* i?86-*-* } } } */

/* Check it to be compiled successfully without any ICE.  */

int a;
unsigned *b;

void foo()
{
  for (unsigned i; i <= a; ++i, ++b)
    ;
}
