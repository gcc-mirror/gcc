/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-require-effective-target alloca } */

void foo(void)
{
 __builtin_calloc (1, 1); /* { dg-warning "ignoring return value of '__builtin_calloc' declared with attribute 'warn_unused_result'" } */
}
