// Build don't link:
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
  
  static_cast <X *> (cp);           // ERROR - lose const
  static_cast <X const *> (cp);
  static_cast <int *> (cp);         // ERROR - lose const
  static_cast <int const *> (cp);
  static_cast <int **> (cp);        // ERROR - lose const
  static_cast <int const **> (cp);  // ERROR - lose const
  static_cast <int *const *> (cp);
  static_cast <int const *const *> (cp);
  
  static_cast <Z *> (yp);
  static_cast <Z const *> (yp);

  static_cast <Z *> (ycp);          // ERROR - lose const
  static_cast <Z const *> (ycp);

  static_cast <Y *> (zp);
  static_cast <Y const *> (zp);

  static_cast <Y *> (zcp);          // ERROR - lose const
  static_cast <Y const *> (zcp);
}
