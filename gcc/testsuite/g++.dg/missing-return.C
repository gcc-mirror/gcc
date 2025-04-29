/* { dg-do compile } */
/* { dg-options "-Wreturn-type -fdump-tree-optimized -O" } */

int foo(int a)
{
} /* { dg-warning "no return statement" } */

/* For targets without traps, it will be an infinite loop */
/* { dg-final { scan-tree-dump "__builtin_unreachable" "optimized" { target trap } } } */
/* { dg-final { scan-tree-dump "goto <" "optimized" { target { ! trap } } } } */
