// { dg-do assemble  }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@lsd.ic.unicamp.br>
// Based on bug report by JDonner <jdonner@schedsys.com>

struct foo {
  static int bar(); // { dg-error "candidate" }
  void bar(int); // { dg-error "foo::bar" }
};

/* gcc emits a hard error without -pedantic, and a warning with
   -pedantic, even in bad1.  */
int (*ok1)() = foo::bar;
void (foo::*bad1)(int) = foo::bar; // { dg-error "assuming pointer" }
// { dg-message "note" "note" { target *-*-* } 16 }

int (*ok2)() = &foo::bar; // ok
void (*bad2)(int) = foo::bar; // { dg-error "no matches" }

void (foo::*ok3)(int) = &foo::bar; // ok
int (foo::*bad3)() = foo::bar; // { dg-error "no matches" }

