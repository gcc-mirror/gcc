/* PR target/59794 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -mno-mmx" } */
/* { dg-skip-if "no MMX vector" { *-*-mingw* } } */

typedef int __v2si __attribute__ ((__vector_size__ (8)));

extern __v2si x;

__v2si
foo (void)
{ /* { dg-warning "MMX vector return without MMX enabled changes the ABI" } */
  return x;
}
