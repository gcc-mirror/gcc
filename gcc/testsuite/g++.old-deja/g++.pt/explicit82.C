// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Sep 2000 <nathan@codesourcery.com>

// Bug 508. We failed to set/clear lastiddecl appropriately for
// operator names.

struct A {};

template <typename N> void foo (A, int);
template <typename N> void operator<< (A, int);

int main()
{ 
        A a;
        operator<< <bool>(a, 0); 
        foo <bool>(a, 0); 
}
