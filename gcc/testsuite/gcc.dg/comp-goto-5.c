/* PR c/32122 */
/* { dg-do compile } */
/* { dg-options "" } */

enum {a=1};
void foo()
{
  goto *
        a; /* { dg-error "computed goto must be pointer type" } */
}

