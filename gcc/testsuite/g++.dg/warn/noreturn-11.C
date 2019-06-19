// PR c++/60364
// { dg-do compile { target c++11 } }

void f1 ();
void f1 [[gnu::noreturn]] ();
void f1 [[noreturn]] ();

void f2 ();
__attribute__((noreturn)) void f2 ();
void f2 [[noreturn]] ();

void f3 ();
void f3 [[gnu::noreturn]] ();
void f3 ();
void f3 [[noreturn]] ();

void f4 ();
void f4 ();
void f4 ();
void f4 [[noreturn]] (); // { dg-error "declared '\\\[\\\[noreturn\\\]\\\]' but its first declaration was not" }

void f5 [[noreturn]] ();
void f5 ();
void f5 ();
void f5 [[noreturn]] ();
