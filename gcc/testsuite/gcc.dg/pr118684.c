/* { dg-do run { target automatic_stack_alignment } } */
/* { dg-options "-O2" } */

typedef int v4si __attribute__((vector_size(16)));
v4si x;
int main ()
{
  int b __attribute__((aligned(16)));
  b = 0;
  x = *(v4si *)&b;
  return 0;
}
