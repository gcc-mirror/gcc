// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Feb 2000 <nathan@acm.org>

// Array of cv T decays to pointer to cv T, and pointer to array of cv T can
// be converted to pointer to cv T. We need to make sure the `cv's don't
// confuse us.

typedef char const *ary_t[2];

void f0 (ary_t const *const &ary)
{
  static_cast <void const *> (ary);
  static_cast <void *> (ary);         // { dg-error "" } casts away const
  (void const *) (ary);
}

void f1 (ary_t *const &ary)
{
  static_cast <void *> (ary);
  static_cast <void const *> (ary);
  (void const *) (ary);
}

void f2 (ary_t const *&ary)
{
  static_cast <void const *> (ary);
  static_cast <void *> (ary);         // { dg-error "" } casts away const
  (void const *) (ary);
}

void f3 (ary_t *&ary)
{
  static_cast <void *> (ary);
  static_cast <void const *> (ary);
  (void const *) (ary);
}
