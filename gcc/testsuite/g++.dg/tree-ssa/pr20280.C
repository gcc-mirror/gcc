// PR c++/20280

// { dg-do compile }

// Gimplification of the COND_EXPR used to fail because it had an
// addressable type, and create_tmp_var rejected that.

struct A
{
    ~A();
};

struct B : A {};

A& foo();

void bar(bool b)
{
    (B&) (b ? foo() : foo());
}

// Make sure bit-fields and addressable types don't cause crashes.
// These were not in the original bug report.

// Added by Alexandre Oliva <aoliva@redhat.com>

// Copyright 2005 Free Software Foundation

struct X
{
  long i : 32, j, k : 32;
};

void g(long&);
void h(const long&);

void f(X &x, bool b)
{
  (b ? x.i : x.j) = 1;
  (b ? x.j : x.k) = 2;
  (b ? x.i : x.k) = 3;

  (void)(b ? x.i : x.j);
  (void)(b ? x.i : x.k);
  (void)(b ? x.j : x.k);

  g (b ? x.i : x.j); // { dg-error "cannot bind bitfield" }
  g (b ? x.i : x.k); // { dg-error "cannot bind bitfield" }
  g (b ? x.j : x.k); // { dg-error "cannot bind bitfield" }

  // It's not entirely clear whether these should be accepted.  The
  // conditional expressions are lvalues for sure, and 8.5.3/5 exempts
  // lvalues for bit-fields, but it's not clear that conditional
  // expressions that are lvalues and that have at least one possible
  // result that is a bit-field lvalue meets this condition.
  h (b ? x.i : x.j);
  h (b ? x.i : x.k);
  h (b ? x.j : x.k);

  (long &)(b ? x.i : x.j); // { dg-error "address of bit-field" }
  (long &)(b ? x.i : x.k); // { dg-error "address of bit-field" }
  (long &)(b ? x.j : x.k); // { dg-error "address of bit-field" }
}
