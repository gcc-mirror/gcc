extern void free (void *);
char *xstrdup (const char *)
  __attribute__((malloc (free), returns_nonnull));

void test_1 (const char *s)
{
  char *p = xstrdup (s);
  free (p);
}

/* Verify that we don't issue -Wanalyzer-possible-null-dereference
   when the allocator has __attribute__((returns_nonnull)).  */

char *test_2 (const char *s)
{
  char *p = xstrdup (s);
  p[0] = 'a'; /* { dg-bogus "possibly-NULL" } */
  return p;
}

void test_3 (const char *s)
{
  char *p = xstrdup (s); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'p'" } */
