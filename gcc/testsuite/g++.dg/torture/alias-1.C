// Verify we don't try to allocate the same stack slot for
// buf1 and buf2 in qux.  While there is a CLOBBER stmt for buf1
// from inlined destructor, the buf1 variable doesn't go out of scope
// until after the baz call.
// { dg-do run }

#include <new>
#include <cstring>
#include <cstdlib>

char *p;
struct S { char buf[128]; S () { memset (buf, ' ', 128); }; ~S () {}; };

__attribute__((noipa)) void
foo (char *x)
{
  p = x;
}

__attribute__((noipa)) int
bar (S *x)
{
  return x->buf[12];
}

__attribute__((noipa)) void
baz (char *x)
{
  S *a = new (p) (S);
  S *b = new (x) (S);
  memset (a->buf, '0', 128);
  memset (b->buf, '1', 128);
  if (bar (a) != '0' || bar (b) != '1')
    abort ();
  b->~S ();
  a->~S ();
}

__attribute__((noipa)) void
qux ()
{
  char buf1[128];
  foo (buf1);
  S *p = new (buf1) (S);
  bar (p);
  p->~S ();
  {
    char buf2[128];
    baz (buf2);
  }
}

int
main ()
{
  qux ();
}
