/* Test for no ICE with an undeclared identifier in an enum in old-style
   parameter decls.  PR 12560.  */
/* { dg-options "-fpermissive -w" } */

foo(c)
     enum { a = b } c;  /* { dg-error "undeclared|for each" } */
{
}
