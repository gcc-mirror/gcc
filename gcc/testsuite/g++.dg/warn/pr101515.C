// PR c++/101515
// { dg-do compile }
// { dg-options "-O1 -Wuninitialized" }

struct S { int j; };
struct T : public S { virtual void h () {} };
struct U { void (*ptr) (); };
typedef void (S::*sp) ();

int
main ()
{
  T t;
  sp x;
  U *xp = (U *) &x;
  if (xp->ptr != ((void (*) ()) (sizeof (void *))))	// { dg-warning "is used uninitialized" }
    return 1;
}
