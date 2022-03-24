/* PR middle-end/100680 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -Wstringop-overread" } */

struct s {
  char a[8];
  int i;
  long l;
};

extern char ea[8];
static char sa[8] = { 1, 2, 3, 4 };

int
test (void)
{
  const struct s *ps = (const struct s *) 0x12345678L;
  if (__builtin_memcmp (ps->a, ps->a, 8))
    return 0;

  if (__builtin_memcmp (ps->a, ea, 8))		/* { dg-bogus "exceeds source size 0" } */
    return 0;

  if (__builtin_memcmp (ps->a, sa, 8))		/* { dg-bogus "exceeds source size 0" } */
    return 0;

  if (__builtin_memcmp (ps->a, "abcdABCD", 8))	/* { dg-bogus "exceeds source size 0" } */
    return 0;

  return 1;
}
