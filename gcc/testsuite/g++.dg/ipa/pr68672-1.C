// PR ipa/68672
// { dg-do compile }
// { dg-options "-O -finline-small-functions -fpartial-inlining --param=partial-inlining-entry-probability=100" }

void f2 (void *);
void *a;
struct C { virtual void m1 (); };
struct D { C *m2 () { if (a) __builtin_abort (); } };
D f1 ();
struct E { int e; ~E () { if (e) f2 (&e); } };
E *b;
struct I { virtual void m3 (); };

void
I::m3 ()
{
  if (a)
    f1 ().m2 ()->m1 ();
  b->~E ();
}
