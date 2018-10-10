/* PR target/87573 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O1 -mmmx -mno-sse" } */

typedef char __v8qi __attribute__((vector_size(8)));

__v8qi e;

void f (void)
{
  e = (__v8qi) {0, 0, 0, 0, 0, 0, 0, 0};
}
