// PR c++/60364
// { dg-do compile { target c++11 } }
// { dg-options "-Wpedantic" }

void f1 [[gnu::noreturn]] ();
void f1 [[noreturn]] ();

void f2 [[noreturn]] ();
void f2 [[gnu::noreturn]] ();

__attribute__((noreturn)) void f3 ();
void f3 [[noreturn]] ();

void f4 [[noreturn]] ();
__attribute__((noreturn)) void f4 ();

__attribute__((noreturn)) void f5 ();
void f5 [[gnu::noreturn]] ();

void f6 [[gnu::noreturn]] ();
__attribute__((noreturn)) void f6 ();
