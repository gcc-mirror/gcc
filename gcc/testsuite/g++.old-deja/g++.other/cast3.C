// { dg-do assemble  }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Dec 1999 <nathan@acm.org>

// static_cast should not cast _away_ constness ([expr.static.cast]/6),
// but nothing bans _adding_ constness. [expr.static.cast]/10 states that a
// pointer of type cv void can be cast to pointer to object type.

struct X;
struct Y {};
struct Z : Y {};

void fn (void *p, void const *cp, Y *yp, Y const *ycp, Z *zp, Z const *zcp)
{
  static_cast <X *> (p);
  static_cast <X const *> (p);
  static_cast <int *> (p);
  static_cast <int const *> (p);
  static_cast <int **> (p);
  static_cast <int const **> (p);
  static_cast <int *const *> (p);
  static_cast <int const *const *> (p);
  
  static_cast <X *> (cp);           // { dg-error "3:.static_cast. from type .const void\\*. to type .X\\*. casts away qualifiers" } lose const
  static_cast <X const *> (cp);
  static_cast <int *> (cp);         // { dg-error "3:.static_cast. from type .const void\\*. to type .int\\*. casts away qualifiers" } lose const
  static_cast <int const *> (cp);
  static_cast <int **> (cp);        // { dg-error "3:.static_cast. from type .const void\\*. to type .int\\*\\*. casts away qualifiers" } lose const
  static_cast <int const **> (cp);  // { dg-error "3:.static_cast. from type .const void\\*. to type .const int\\*\\*. casts away qualifiers" } lose const
  static_cast <int *const *> (cp);
  static_cast <int const *const *> (cp);
  
  static_cast <Z *> (yp);
  static_cast <Z const *> (yp);

  static_cast <Z *> (ycp);          // { dg-error "3:.static_cast. from type .const Y\\*. to type .Z\\*. casts away qualifiers" } lose const
  static_cast <Z const *> (ycp);

  static_cast <Y *> (zp);
  static_cast <Y const *> (zp);

  static_cast <Y *> (zcp);          // { dg-error "3:invalid .static_cast. from type .const Z\\*. to type .Y\\*." } lose const
  static_cast <Y const *> (zcp);
}
