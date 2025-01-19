// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Nov 2000 <nathan@codesourcery.com>


// bug 721, we died horribly when export was used wrongly

struct test {
int export(void);   // { dg-error "" } parse error
};

int test::export(void) // { dg-error "" } parse error
{
return 0;
}

template <class T> class Y;
export template <class T> class X;  // { dg-message "export" } export not implemented
