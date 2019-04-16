// PR debug/89006
// { dg-do compile }
// { dg-options "-O2 -g -w" }
// { dg-additional-options "-fPIC" { target fpic } }
// { dg-bogus "non-delegitimized UNSPEC UNSPEC_SET_GOT" "" { target { i?86-*-* x86_64-*-* } } 0 }

struct A { A (bool); };

static void
foo (const char *x)
{
  new A (x);
}

void
bar ()
{
  foo ("foo");
  foo ("bar");
}
