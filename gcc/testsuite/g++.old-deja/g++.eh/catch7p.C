// { dg-do run  }
// { dg-options "-w" }
// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Aug 1999 <nathan@acm.org>

// We cannot catch an ambiguous base class.
// -- public, << private, == virtual

// different levels
// F--D--B--A
//    +--C--A
// +--E--A


struct A { int m; virtual ~A(){}};
struct B : A { int m; };
struct C : A { int m; };
struct D : B, C { int m; };
struct E : A { int m; };
struct F : D, E { int m; };

void fna(A *obj) { throw obj; }
void fnb(B *obj) { throw obj; }
void fnc(C *obj) { throw obj; }
void fnd(D *obj) { throw obj; }
void fne(E *obj) { throw obj; }
void fnf(F *obj) { throw obj; }

extern "C" void abort();

void check(F *f)
{
  int caught;
  
  // try with whole object
  caught = 0;
  try { fnf(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(F *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fnf(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(E *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fnf(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(D *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fnf(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(B *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fnf(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(C *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  // try with D object
  caught = 0;
  try { fnd(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(D *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fnd(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(B *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fnd(f); }
  catch(A *p) { abort(); } // A is ambiguous
  catch(C *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  // try with E object
  caught = 0;
  try { fne(f); }
  catch(A *p) { caught = 1; if (p != (E *)f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fne(f); }
  catch(E *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();

  caught = 0;
  try { fne(f); }
  catch(F *p) { abort(); }
  catch(...) { caught = 1; }
  if (!caught) abort();

  // try with an A object
  caught = 0;
  try { fna((B *)f); }
  catch(B *p) { abort(); } // throw type is static type
  catch(A *p) { caught = 1; if (p != (B *)f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();
  
  caught = 0;
  try { fna((C *)f); }
  catch(C *p) { abort(); } // throw type is static type
  catch(A *p) { caught = 1; if (p != (C *)f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();
  
  caught = 0;
  try { fna((E *)f); }
  catch(E *p) { abort(); } // throw type is static type
  catch(A *p) { caught = 1; if (p != (E *)f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();
  
  // try with B object
  caught = 0;
  try { fnb((B *)f); }
  catch(A *p) { caught = 1; if (p != (B *)f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();
  
  caught = 0;
  try { fnb((B *)f); }
  catch(B *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();
  
  caught = 0;
  try { fnb((B *)f); }
  catch(C *p) { abort(); }
  catch(D *p) { abort(); }
  catch(...) { caught =1; }
  if (!caught) abort();
  
  // try with C object
  caught = 0;
  try { fnc((C *)f); }
  catch(A *p) { caught = 1; if (p != (C *)f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();
  
  caught = 0;
  try { fnc((C *)f); }
  catch(C *p) { caught = 1; if (p != f) abort();}
  catch(...) { abort(); }
  if (!caught) abort();
  
  caught = 0;
  try { fnc((C *)f); }
  catch(B *p) { abort(); }
  catch(D *p) { abort(); }
  catch(...) { caught =1; }
  if (!caught) abort();
  
  return;
}

int main ()
{
  F f;
  check (&f); // try with an object
  check ((F *)0); // try with no object
  
  return 0;
}
