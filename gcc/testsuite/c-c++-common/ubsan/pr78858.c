/* PR middle-end/78858 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -Wnonnull" } */

void
foo (char **x, const char *y)
{
  *x = (char *) __builtin_malloc (__builtin_strlen (y) + 1);	/* { dg-bogus "argument 1 null where non-null expected" } */
  __builtin_strcpy (*x, y);
}
