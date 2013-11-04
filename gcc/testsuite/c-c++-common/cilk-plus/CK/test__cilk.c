/* { dg-do compile } */
/* { dg-do run  { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  if (__cilk == 200)
   return 0; 
  return 1;
}
