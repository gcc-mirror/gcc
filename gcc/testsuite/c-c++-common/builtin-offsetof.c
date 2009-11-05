// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/38699
// { dg-options "-Warray-bounds" }
// { dg-do compile }

struct A
{
  const char *p;
};

struct B
{
    char p[10];
    struct A a;
};

void
f0 ()
{
  __builtin_offsetof(struct A, p); // OK
  __builtin_offsetof(struct A, p[0]); // { dg-error "non constant address" }
  __builtin_offsetof(struct B, p[0]); // OK
  __builtin_offsetof(struct B, p[9]); // OK
  __builtin_offsetof(struct B, p[10]); // OK
  __builtin_offsetof(struct B, a.p); // OK
  __builtin_offsetof(struct B, p[0]); // OK
  __builtin_offsetof(struct B, a.p[0]); // { dg-error "non constant address" }
}
