// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 May 2000 <nathan@codesourcery.com>

// we should be able to catch a base a virtual, provided it is accessible by at
// least one public path
// -- public, << private, == virtual
// E--B<<==A
// +--C--==A
// +--D<<==A

struct A {};
struct B : private virtual A {};
struct C : virtual A {};
struct D : private virtual A {};
struct E : public B, public C, public D {};

extern "C" void abort ();

void fne (E *e)
{
  throw e;
}

void check(E *e)
{
  int caught;
  
  caught = 0;
  try { fne(e); }
  catch(A *p) { caught = 1; if (p != e) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fne(e); }
  catch(B *p) { caught = 1; if (p != e) abort();}
  catch(...) { abort (); }
  if (!caught) abort();

  caught = 0;
  try { fne(e); }
  catch(C *p) { caught = 1; if (p != e) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fne(e); }
  catch(D *p) { caught = 1; if (p != e) abort ();}
  catch(...) { abort (); }
  if (!caught) abort();

  return;
}

int main ()
{
  E e;
  
  check (&e);
  check ((E *)0);

  return 0;
}
