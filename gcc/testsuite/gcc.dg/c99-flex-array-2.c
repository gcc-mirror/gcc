/* PR c/5623 */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

struct blah {
   int   number;
   char  array[];
};

void foo(void)
{
   struct blah b;
   b.array = "hi";  /* { dg-error "invalid use of flexible array member" } */
}
