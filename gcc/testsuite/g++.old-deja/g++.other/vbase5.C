// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Jan 2001 <nathan@codesourcery.com>

// Bug 1701. building a vbase path was not using the shortest number of
// vbases. Normally that's just a pessimization, unfortunately during
// constructoring it leads to uninitialized reads.

extern "C" int printf (...);

int fail = 0;

/*{{{  struct Base*/
struct Base
{
  unsigned m;
  static Base *addr;
  
  Base ();
  virtual ~Base ();
};
/*}}}*/
Base *Base::addr;
/*{{{  Base::Base ()*/
Base::Base ()
{
  printf ("Base (%u) ctor %x\n", sizeof (Base), this);
  if (fail) ;
  else if (addr)
    fail = 1;
  else
    addr = this;
}
/*}}}*/
/*{{{  Base::~Base ()*/
Base::~Base ()
{
  printf ("Base dtor %x\n", this);
  if (fail)
    ;
  else if (this != addr)
    fail = 2;
  else
    addr = 0;
}
/*}}}*/

/*{{{  struct M10 : virtual Base*/
struct M10 : virtual Base
{
  int m;
  static M10 *addr;
  
  M10 ();
  virtual ~M10 ();
};
/*}}}*/
M10 *M10::addr;
/*{{{  M10::M10 ()*/
M10::M10 ()
{
  printf ("M10 (%u) ctor %x\n", sizeof (M10), this);
  if (fail) ;
  else if (addr)
    fail = 3;
  else
    addr = this;
}
/*}}}*/
/*{{{  M10::~M10 ()*/
M10::~M10 ()
{
  printf ("M10 dtor %x\n", this);
  if (fail)
    ;
  else if (this != addr)
    fail = 4;
  else
    addr = 0;
}
/*}}}*/

/*{{{  struct M4 : virtual Base, virtual M10*/
struct M4 : virtual Base, virtual M10
{
  int m;
  static M4 *addr;
  
  M4 ();
  virtual ~M4 ();
};
/*}}}*/
M4 *M4::addr;
/*{{{  M4::M4 ()*/
M4::M4 ()
{
  printf ("M4 (%u) ctor %x\n", sizeof (M4), this);
  if (fail) ;
  else if (addr)
    fail = 5;
  else
    addr = this;
}
/*}}}*/
/*{{{  M4::~M4 ()*/
M4::~M4 ()
{
  printf ("M4 dtor %x\n", this);
  if (fail)
    ;
  else if (this != addr)
    fail = 6;
  else
    addr = 0;
}
/*}}}*/

/*{{{  struct M5 : M4*/
struct M5 : M4
{
  int m;
  static M5 *addr;
  
  M5 ();
  virtual ~M5 ();
};
/*}}}*/
M5 *M5::addr;
/*{{{  M5::M5 ()*/
M5::M5 ()
{
  printf ("M5 (%u) ctor %x\n", sizeof (M5), this);
  if (fail) ;
  else if (addr)
    fail = 7;
  else
    addr = this;
}
/*}}}*/
/*{{{  M5::~M5 ()*/
M5::~M5 ()
{
  printf ("M5 dtor %x\n", this);
  if (fail)
    ;
  else if (this != addr)
    fail = 8;
  else
    addr = 0;
}
/*}}}*/

/*{{{  struct M9 : M5, virtual M10*/
struct M9 : M5, virtual M10
{
  int m;
  static M9 *addr;
  
  M9 ();
  virtual ~M9 ();
};
/*}}}*/
M9 *M9::addr;
/*{{{  M9::M9 ()*/
M9::M9 ()
{
  printf ("M9 (%u), ctor %x\n", sizeof (M9), this);
  if (fail) ;
  else if (addr)
    fail = 9;
  else
    addr = this;
}
/*}}}*/
/*{{{  M9::~M9 ()*/
M9::~M9 ()
{
  printf ("M9 dtor %x\n", this);
  if (fail)
    ;
  else if (this != addr)
    fail = 10;
  else
    addr = 0;
}
/*}}}*/

int main ()
{
  M9 *m9;
  Base *r;
  
  m9 = new M9 ();
  r = m9;
  if (fail)
    return fail;
  void *top = dynamic_cast <void *> (r);
  if (top != m9)
    return 20;
  r->~Base ();
  
  return fail;
}
