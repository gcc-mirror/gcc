/* { dg-do compile } */
/* { dg-options "-Wreturn-type -fdump-tree-optimized" } */

int foo(int a)
{
} /* { dg-warning "no return statement" } */

/* { dg-final { scan-tree-dump "__builtin_unreachable" "optimized" } } */
