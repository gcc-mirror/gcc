#include <new>
#include <cstddef>

struct foo
{
  foo (int i) : m_i (i) {} // { dg-message "argument 'this' of 'foo::foo\\(int\\)' must be non-null" "note" }

  int get () const { return m_i; } // { dg-message "argument 'this' of '\[^\n\]*' must be non-null" "note" }
  
  int meth_1 (int, void *ptr) __attribute__((nonnull)); // { dg-message "argument 2 of '\[^\n\]*' must be non-null" "note" }
  int meth_2 (int, void *ptr) __attribute__((nonnull(3))); // { dg-message "argument 2 of '\[^\n\]*' must be non-null" "note" }

  int m_i;
};

void test_1 (void)
{
  foo *p = new(NULL) foo (42); // { dg-warning "non-null expected" "warning" }
  // { dg-message "argument 'this' \\(\[^\n\]*\\) NULL where non-null expected" "final event" { target *-*-* } .-1 }
}

int test_2 (void)
{
  foo *p = NULL;
  return p->get (); // { dg-warning "non-null expected" "warning" }
  // { dg-message "argument 'this' \\('p'\\) NULL where non-null expected" "final event" { target *-*-* } .-1 }
}

int test_meth_1 (foo *f)
{
  return f->meth_1 (42, NULL); // { dg-warning "non-null expected" "warning" }
  // { dg-message "argument 2 NULL where non-null expected" "final event" { target *-*-* } .-1 }
}

int test_meth_2 (foo *f)
{
  return f->meth_2 (42, NULL); // { dg-warning "non-null expected" "warning" }
  // { dg-message "argument 2 NULL where non-null expected" "final event" { target *-*-* } .-1 }
}
