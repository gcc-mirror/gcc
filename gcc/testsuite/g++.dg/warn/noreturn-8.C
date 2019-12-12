// PR c++/60364
// { dg-do compile { target c++11 } }
// { dg-options "-Wpedantic" }

void f (); // { dg-message "previous declaration" }
void f [[noreturn]] (); // { dg-error "declared '\\\[\\\[noreturn\\\]\\\]' but its first declaration was not" }

void f2 ();
void f2 [[gnu::noreturn]] ();

void f3 ();
__attribute__((noreturn)) void f3 ();

void f4 () { __builtin_abort (); } // { dg-message "previous declaration" }
void f4 [[noreturn]] (); // { dg-error "declared '\\\[\\\[noreturn\\\]\\\]' but its first declaration was not" }

void f5 () { __builtin_abort (); }
void f5 [[gnu::noreturn]] ();

void f6 () { __builtin_abort (); }
__attribute__((noreturn)) void f6 ();
