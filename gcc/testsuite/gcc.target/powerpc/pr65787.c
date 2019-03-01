/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */
/* { dg-final { scan-assembler "xxsldwi \[0-9\]*,\[0-9\]*,\[0-9\]*,3" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

/* This test verifies that a vector extract operand properly has its
   lane changed by the swap optimization.  Element 2 of LE corresponds
   to element 1 of BE.  When doublewords are swapped, this becomes
   element 3 of BE, so we need to shift the vector left by 3 words
   to be able to extract the correct value from BE element zero.  */

typedef float  v4f32 __attribute__ ((__vector_size__ (16)));

void foo (float);
extern v4f32 x, y;

int main() {
  v4f32 z = x + y;
  foo (z[2]);
}
