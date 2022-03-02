extern void free (void *);

struct foo
{
  int m_int;
};

extern void foo_release (struct foo *);
extern struct foo *foo_acquire (void)
  __attribute__ ((malloc (foo_release)));
extern void use_foo (const struct foo *)
  __attribute__((nonnull));

extern struct foo *compatible_alloc (void)
  __attribute__ ((malloc (__builtin_free)));

extern struct foo *compatible_alloc2 (void)
  __attribute__ ((malloc (free)));

void test_1 (void)
{
  struct foo *p = foo_acquire ();
  foo_release (p);
}

void test_2 (void)
{
  struct foo *p = foo_acquire (); /* { dg-message "this call could return NULL" } */
  p->m_int = 42; /* { dg-warning "dereference of possibly-NULL 'p'" } */
  foo_release (p);
}

void test_2a (void)
{
  struct foo *p = foo_acquire (); /* { dg-message "this call could return NULL" } */
  use_foo (p); /* { dg-warning "use of possibly-NULL 'p' where non-null expected" } */
  foo_release (p);
}

void test_3 (void)
{
  struct foo *p = foo_acquire (); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'p'" } */

void test_4 (struct foo *p)
{
  foo_release (p);
  foo_release (p); /* { dg-warning "double-'foo_release' of 'p'" } */
}

void test_4a (void)
{
  struct foo *p = foo_acquire ();
  foo_release (p);
  foo_release (p); /* { dg-warning "double-'foo_release' of 'p'" } */
}

void test_5 (void)
{
  struct foo *p = foo_acquire (); /* { dg-message "allocated here \\(expects deallocation with 'foo_release'\\)" } */
  free (p); /* { dg-warning "'p' should have been deallocated with 'foo_release' but was deallocated with 'free'" } */
}

void test_6 (struct foo *p)
{
  foo_release (p);
  free (p); // TODO: double-release warning!
}

void test_7 ()
{
  struct foo f;
  foo_release (&f); /* { dg-warning "on the stack" "analyzer" } */
  /* { dg-warning "'foo_release' called on unallocated object 'f'" "non-analyzer" { target *-*-* } .-1 } */
}

int test_8 (struct foo *p)
{
  foo_release (p);
  return p->m_int; /* { dg-warning "use after 'foo_release' of 'p'" } */
}

/* Recognize that __builtin_free and free are the same thing.  */
void test_9 (void)
{
  struct foo *p = compatible_alloc ();
  free (p);
}

void test_10 (void)
{
  struct foo *p = compatible_alloc2 ();
  __builtin_free (p);
}
