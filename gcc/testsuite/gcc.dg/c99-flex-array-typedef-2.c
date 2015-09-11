/* Test for invalid uses of flexible array members.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

typedef char A[];

struct S {
   int n;
   A a;
};

void
foo (void)
{
  struct S s;
  s.a = "abc";  /* { dg-error "invalid use of flexible array member" } */
}
