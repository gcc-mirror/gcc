/* Verify that we detect double-free in expressions involving uintptr
   for various states of the store/binding_clusters.  */
   
struct foo { char *expr; };

void
test_param (__UINTPTR_TYPE__ i)
{
  __builtin_free (((struct foo *)i)->expr);
  __builtin_free (((struct foo *)i)->expr); /* { dg-warning "double-'free'" } */
}

void
test_local (__UINTPTR_TYPE__ i)
{
  struct foo *f = (struct foo *)i;
  __builtin_free (f->expr);
  __builtin_free (f->expr); /* { dg-warning "double-'free'" } */
}

void
test_voidptr (void *i)
{
  __builtin_free (((struct foo *)i)->expr);
  __builtin_free (((struct foo *)i)->expr);  /* { dg-warning "double-'free'" } */
}

void
test_write_to_unrelated_cluster (__UINTPTR_TYPE__ i)
{
  char buf[8];
  buf[0] = 42;
  __builtin_free (((struct foo *)i)->expr);
  __builtin_free (((struct foo *)i)->expr);  /* { dg-warning "double-'free'" } */
  buf[1] = buf[0];
}

void
test_after_write (__UINTPTR_TYPE__ i, char *p)
{
  ((struct foo *)i)->expr = p;
  __builtin_free (((struct foo *)i)->expr);
  __builtin_free (((struct foo *)i)->expr); /* { dg-warning "double-'free'" } */
}

void
test_ssa_name_keeps_it_live (__UINTPTR_TYPE__ i)
{
  char *a = ((struct foo *)i)->expr;
  __builtin_free (a);
  __builtin_free (((struct foo *)i)->expr); /* { dg-warning "double-'free'" } */
  __builtin_free (a);
}

extern struct foo *
make_foo (void);

void
test_from_fn (__UINTPTR_TYPE__ i)
{
  ((struct foo *)i)->expr = (char *)make_foo ();
  __builtin_free (((struct foo *)i)->expr);
  __builtin_free (((struct foo *)i)->expr); /* { dg-warning "double-'free'" } */
}

