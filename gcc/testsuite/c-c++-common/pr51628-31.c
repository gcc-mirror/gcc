/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

typedef int v4si __attribute__((vector_size(16)));
struct X
{
  v4si x;
} __attribute__((packed)) x;

int *
foo()
{
  return &x.x[1];
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
}
