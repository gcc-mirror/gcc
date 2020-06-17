/* { dg-do compile } */
/* { dg-options "-O2 -Wrestrict" } */

static char *
str_numth(char *dest, char *num, int type)
{
  if (dest != num)
    __builtin_strcpy(dest, num); /* { dg-bogus "is the same" } */
  __builtin_strcat(dest, "foo");
  return dest;
}

void
DCH_to_char(char *in, char *out, int collid)
{
  char *s = out;
  str_numth(s, s, 42);
}
