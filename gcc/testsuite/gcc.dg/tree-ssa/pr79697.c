/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lower -fdump-tree-cddce-details -fdump-tree-optimized" } */

void f(void)
{
  __builtin_strdup ("abc"); /* { dg-warning "ignoring return value of '__builtin_strdup' declared with attribute 'warn_unused_result'" } */
}

void g(void)
{
  __builtin_strndup ("abc", 3); /* { dg-warning "ignoring return value of '__builtin_strndup' declared with attribute 'warn_unused_result'" } */
}

void h(void)
{
  __builtin_realloc (0, 10); /* { dg-warning "ignoring return value of '__builtin_realloc' declared with attribute 'warn_unused_result'" } */
}

/* { dg-final { scan-tree-dump "Deleting : __builtin_strdup" "cddce1" } } */
/* { dg-final { scan-tree-dump "Deleting : __builtin_strndup" "cddce1" } } */
/* { dg-final { scan-tree-dump "__builtin_malloc" "lower" } } */
