// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Aug 2000 <nathan@codesourcery.com>

// bug 39. We'd generated bogus errors when using a typedef of a nested class.

struct X1
{
  typedef struct {
  } MyStruct1;
  typedef struct M2 {
  } MyStruct2;
};

X1::MyStruct1 foo()
{
X1::MyStruct1 m1;
return m1;
}

X1::MyStruct2 baz()
{
X1::MyStruct2 m1;
return m1;
}
