// Build don't link:
// Skip if not target: sparc-*-*

// Special g++ Options: -ansi -pedantic-errors -Wcast-align

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Dec 1999 <nathan@acm.org>

// converting a T * to void * does not need a complete T, and doesn't
// increase alignment requirements.

struct X;
struct Y;
struct Z {double m;};

void f3 (X *xp, Z *zp)
{
  (void *)xp;
  (void *)zp;
  (Y *)xp;
  (Y *)zp;
  (Z *)xp;
}

void f4 (char *ptr)
{
  (Z *)ptr;           // WARNING - alignment
}
