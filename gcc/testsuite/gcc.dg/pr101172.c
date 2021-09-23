/* PR middle-end/101172 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

union U
{
  int a[3];
  struct
  {
    int a : 3;
    struct this_struct var;	/* { dg-error "field 'var' has incomplete type" } */
  } b;
};

const union U hello = {.a = {1, 2, 3}};

void foo()
{
  int x = hello.b.a;
}
