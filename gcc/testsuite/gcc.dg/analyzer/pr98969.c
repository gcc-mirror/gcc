struct foo
{
  char *expr;
};

void
test_1 (__UINTPTR_TYPE__ i)
{
  struct foo *f = (struct foo *)i;
  f->expr = __builtin_malloc (1024);
} /* { dg-bogus "leak" } */

void
test_2 (__UINTPTR_TYPE__ i)
{
  __builtin_free (((struct foo *)i)->expr);
  __builtin_free (((struct foo *)i)->expr); /* { dg-warning "double-'free' of '\\*\\(\\(struct foo \\*\\)i\\)\\.expr'" } */
}

void
test_3 (void *p)
{
  void **q = (void **)p;
  *q = __builtin_malloc (1024);  
}
