// PR c++/70448
// { dg-do compile }
// { dg-options "-Wall" }

typedef __typeof__ (sizeof 0) size_t;
void *operator new (size_t, void *p) { return p; }
void *operator new[] (size_t, void *p) { return p; }
struct S { size_t s; };
void bar (S *);

void
foo (unsigned int s)
{
  char t[sizeof (S) + s];
  S *f = new (t) S;
  bar (f);
  f = new (t) S[1];
  bar (f);
}
