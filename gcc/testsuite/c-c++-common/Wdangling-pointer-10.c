/* { dg-do compile } */
/* { dg-options "-O2 -Wdangling-pointer" } */

struct S {
  int x;
};

void g (int **p)
{
  struct S s = {};
  *p = &s.x; /* { dg-warning "address of local variable" } */
}
