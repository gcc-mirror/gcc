/* Reduced from https://sourceware.org/bugzilla/show_bug.cgi?id=20978 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wnonnull" } */

int
foo (const char *name)
{
  if (name)
    return 6;
  return __builtin_strlen (name);	/* { dg-warning "argument 1 null where non-null expected" } */
}
