/* Test for array of incomplete structure type - Zack Weinberg in
   <http://gcc.gnu.org/ml/gcc-patches/2004-08/msg00108.html>.  */
/* { dg-do compile } */
/* { dg-options "" } */

struct foo;

void
f (void)
{
  struct foo { int a; int b; };
}

struct foo array[5]; /* { dg-error "array type has incomplete element type" } */
