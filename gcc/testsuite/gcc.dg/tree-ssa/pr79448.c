/* PR middle-end/79448 - unhelpful -Wformat-truncation=2 warning
   Verify that there's no warning without optimization.
   { dg-do compile }
   { dg-options "-Wall -Wformat -Wformat-truncation=2" } */

typedef __SIZE_TYPE__ size_t;

extern int
snprintf (char*, size_t, const char*, ...);

char*
gettext (char*);

char*
fill (char *buf, size_t len, int count)
{
  if (snprintf (buf, len, "%s: %d", gettext ("count"), count) >= len)  /* { dg-bogus "directive output of 2 bytes causes result to exceed .INT_MAX." */
    return 0;

  return buf;
}
