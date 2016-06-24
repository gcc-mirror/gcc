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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { !  vect_no_align } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { ! vect_no_align } } } } */


