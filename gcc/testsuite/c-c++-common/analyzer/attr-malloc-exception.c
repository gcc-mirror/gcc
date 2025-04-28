/* { dg-additional-options "-fexceptions" } */

extern void free (void *);

/* Not marked "nothrow", so assume it could throw.  */
char *xstrdup (const char *)
  __attribute__((malloc (free), returns_nonnull));

void test_1 (const char *s, const char *t)
{
  char *p = xstrdup (s); /* { dg-message "allocated here" } */
  char *q = xstrdup (t); /* { dg-warning "leak of 'p'" } */
  /* { dg-message "if .* throws an exception\.\.\." "" { target *-*-* } .-1 } */

  free (q);
  free (p);
}
