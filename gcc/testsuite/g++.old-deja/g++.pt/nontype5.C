// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Jan 2001 <nathan@codesourcery.com>

// Bug 1509. We ICE'd on trying to coerce a non-type template parm
// that wouldn't.

template<class T>
struct A {
typedef int F();
};

template<class T, typename A<T>::F f>
struct B {
static int g() { return f(); };
};

int f() { return 0; }

int main() {
return B<int,&f>::g();  // { dg-error "" } could not convert arg
}
