// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Oct 2005 <nathan@codesourcery.com>

// PR 22604
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

struct A
{
  void foo();  // { dg-message "previous" }
  virtual void foo();  // { dg-error "cannot be overloaded" }
};
