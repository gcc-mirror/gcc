/* { dg-do compile } */
/* { dg-options "-O2 -Wstringop-overflow" } */

void
foo (char *a)
{
  char b[16] = "abcdefg";
  __builtin_strncpy (a, b, __builtin_strlen (b));	/* { dg-warning "specified bound depends on the length of the source argument" } */
}

void
bar (char *a)
{
  char b[16] = "abcdefg";
  __builtin_strncpy (a, b, __builtin_strnlen (b, 8));	/* { dg-warning "specified bound depends on the length of the source argument" } */
}

void
baz (char *a)
{
  char b[16] = "abcdefg";
  __builtin_strncpy (a, b, __builtin_strnlen (b, 7));	/* { dg-bogus "specified bound depends on the length of the source argument" } */
}

void fill (char *);

void
qux (char *a)
{
  char b[16];
  fill (b);
  __builtin_memcpy (b, "abcdefg", 7);
  __builtin_strncpy (a, b, __builtin_strnlen (b, 8));	/* { dg-bogus "specified bound depends on the length of the source argument" } */
}
