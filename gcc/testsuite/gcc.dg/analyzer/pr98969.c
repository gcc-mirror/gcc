struct foo
{
  char *expr;
};

void
test_1 (long int i)
{
  struct foo *f = (struct foo *)i;
  f->expr = __builtin_malloc (1024);
} /* { dg-bogus "leak" "PR analyzer/98969" { xfail *-*-* } } */

void
test_2 (long int i)
{
  __builtin_free (((struct foo *)i)->expr);
  __builtin_free (((struct foo *)i)->expr); /* { dg-warning "double-'free' of '\\*\\(\\(struct foo \\*\\)i\\)\\.expr'" } */
}
