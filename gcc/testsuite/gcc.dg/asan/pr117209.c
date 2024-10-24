/* PR sanitizer/117209 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=address" } */

struct A { char a; };
void foo (void);
__attribute__((returns_twice, const)) int bar (struct A);

void
baz (struct A *x, int *y, int z)
{
  if (z)
    foo (); 
  *y = bar (*x);
}
