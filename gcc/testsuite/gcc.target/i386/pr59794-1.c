/* PR target/59794 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -mno-mmx" } */
/* { dg-skip-if "no MMX vector" { *-*-mingw* } } */

typedef int __v2si __attribute__ ((__vector_size__ (8)));

extern __v2si x;

extern void bar (__v2si);
void
foo (void)
{
  bar (x); /* { dg-message "warning: MMX vector argument without MMX enabled changes the ABI" } */
}
