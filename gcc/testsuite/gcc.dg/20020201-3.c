/* This testcase ICEd because a SFmode variable was given a MMX register
   for which there is no movsf exists.  */
/* { dg-do compile { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-options "-O2 -march=i686 -mmmx -fno-strict-aliasing" } */

struct A { unsigned int a, b; };

void foo (struct A *x, int y, int z)
{
   const float d = 1.0;
   float e = (float) y + z;

   x->a = *(unsigned int *) &d;
   x->b = *(unsigned int *) &e;
}
