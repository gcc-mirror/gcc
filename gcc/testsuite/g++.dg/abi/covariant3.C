// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Dec 2003 <nathan@codesourcery.com>
// Origin:  grigory@stl.sarov.ru

// PR c++/13118. Missing covariant thunk.

struct c0 {};
struct c1 : virtual c0 {
  virtual c0* f6();
};

struct c5 {
    virtual void foo();
};

struct c10 : virtual c1 {
    virtual void foo();
};

struct c1a : c1 {}; // disambiguation

struct c11 : virtual c10, c1a {
  int i;
  virtual c1* f6 () = 0;
};

struct c18 : c5, virtual c1 {
    virtual void bar();
};

struct c28 : virtual c0, virtual c11 {
    virtual c18* f6();
};

c0 *c1::f6 () {}
void c5::foo () {}
void c10::foo () {}
void c18::bar () {}

c18 ret;

c18 *c28::f6 ()
{
  return &ret;
}

bool check_c1 (c1 *ptr)
{
  c0 *r = ptr->f6 ();
  return r != &ret;
}
bool check_c10 (c10 *ptr)
{
  c0 *r = ptr->f6 ();
  return r != &ret;
}
bool check_c11 (c11 *ptr)
{
  c1 *r = ptr->f6 ();
  return r != &ret;
}
bool check_c28 (c28 *ptr)
{
  c18 *r = ptr->f6 ();
  return r != &ret;
}

int main ()
{
  c28 obj;
  
  if (check_c1 (static_cast<c1a *> (&obj)))
    return 1;
  if (check_c1 (static_cast<c10 *> (&obj)))
    return 2;
  if (check_c10 (&obj))
    return 3;
  if (check_c11 (&obj))
    return 4;
  if (check_c28 (&obj))
    return 5;
  return 0;
}
