// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Feb 2000 <nathan@acm.org>

// vtable construction reorganisation broke this

// execution test

#include <stdio.h>

static int fail = 0;

void bad (char const *name)
{
  printf ("Bad %s\n", name);
  fail = 1;
}
void ok (char const *name)
{
  printf ("Ok %s\n", name);
}

struct Core
{
  virtual ~Core ();
  virtual void Wibble () {bad (__PRETTY_FUNCTION__);}
  virtual void Wobble () {bad (__PRETTY_FUNCTION__);}
  virtual void Bogus  () {bad (__PRETTY_FUNCTION__);}
};
struct Side
{
  virtual ~Side ();
  virtual void Arfle () {bad (__PRETTY_FUNCTION__);}
  virtual void Barfle () {bad (__PRETTY_FUNCTION__);}
  virtual void Gloop () {bad (__PRETTY_FUNCTION__);}
  virtual void Glorp () {bad (__PRETTY_FUNCTION__);}
  virtual void Glump () {bad (__PRETTY_FUNCTION__);}
  virtual void Bogus  () {bad (__PRETTY_FUNCTION__);}  
};
struct Base : Core
{
  virtual ~Base ();
  virtual void Bink () {bad (__PRETTY_FUNCTION__);}
  virtual void Bonk () {bad (__PRETTY_FUNCTION__);}
  virtual void  Bogus () {bad (__PRETTY_FUNCTION__);}  
};
struct Multi : Base, Side
{
  virtual ~Multi ();
  virtual void Stomped () {ok (__PRETTY_FUNCTION__);}
  virtual void Bunk () {bad (__PRETTY_FUNCTION__);}
  virtual void Bogus () {bad (__PRETTY_FUNCTION__);}  
};
struct Trail : Multi
{
  virtual ~Trail ();
};
Core::~Core () {}
Side::~Side () {}
Base::~Base () {}
Multi::~Multi () {}
Trail::~Trail () {}

void foo (Multi *ptr)
{
  ptr->Stomped ();
}

int main ()
{
  Multi m;
  Trail t;
  
  foo (&m);
  foo (&t);
  return fail != 0;
}
