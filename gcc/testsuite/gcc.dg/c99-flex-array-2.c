/* PR c/5623 */
/* { dg-do compile } */

struct blah {
   int   number;
   char  array[];
};

void foo(void)
{
   struct blah b;
   b.array = "hi";  /* { dg-error "invalid use of flexible array member" } */
}
