/* PR c/84953 */
/* { dg-do compile } */

char *strpbrk (const char *, const char *);

char *
test (char *p)
{
  p = strpbrk (p, "");	/* { dg-bogus "assignment discards 'const' qualifier from pointer target type" } */
  return p;
}
