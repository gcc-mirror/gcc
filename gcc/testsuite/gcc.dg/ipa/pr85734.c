/* { dg-do compile } */
/* { dg-options "-O2 -Wsuggest-attribute=malloc -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

__attribute__((noinline))
static void *f1(__SIZE_TYPE__ sz) /* { dg-bogus "function might be candidate for attribute 'malloc'" } */
{
  return __builtin_malloc (sz);
}

__attribute__((noinline))
static void *f2(__SIZE_TYPE__ sz) /* { dg-bogus "function might be candidate for attribute 'malloc'" } */
{
  return f1 (sz);
}

void *f3(__SIZE_TYPE__ sz) /* { dg-warning "function might be candidate for attribute 'malloc'" } */
{
  return f2(sz);
}
