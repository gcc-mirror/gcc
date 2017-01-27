/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdisable-tree-cunrolli" } */

struct vector
{
  vector() : x(0), y(0), z(0) { }
  float x,y,z;
};

struct Foo
{
  int dummy;
  /* Misaligned access.  */
  vector array_of_vectors[4];
};

Foo foo;

int main() { }

/* On older powerpc hardware (POWER7 and earlier), the default flag
   -mno-allow-movmisalign prevents vectorization.  On POWER8 and later,
   when vect_hw_misalign is true, vectorization occurs.  For other
   targets, ! vect_no_align is a sufficient test.  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { { { !  vect_no_align } && { ! powerpc*-*-* } } || { powerpc*-*-* && vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { { { ! vect_no_align } && { ! powerpc*-*-* } } || { powerpc*-*-* && vect_hw_misalign } } } } } */


