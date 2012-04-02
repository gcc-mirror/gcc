// { dg-do compile  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Feb 2001 <nathan@codesourcery.com>

// More from bug 1617.  The resolution of DR 214 implies that the below
// call to Foo is ambiguous.
//
// The type transformation (on the function parameter of Foo) allowed
// in the context of partial ordering of the Foo template overloads is
// the following ([temp.deduct.partial]/5):
//
//     Before the partial ordering is done, certain transformations
//     are performed on the types used for partial ordering:
//
//       - If P is a reference type, P is replaced by the type
//         referred to.
//
//       - If A is a reference type, A is replaced by the type
//         referred to.
//
// It follows that we are not allowed to apply array-to-pointer
// decay conversion to the type of the function parameter
// 'char const (&)[I]'.  So the two Foo specializations should
// be considered unrelated.  Thus the partial ordering of the two
// Foo specializations should fail.

template <typename T> int Foo (T const *) {return 1;}
template <unsigned I> int Foo (char const (&)[I]) {return 2;}

int main ()
{
  return Foo ("a") != 2; // { dg-error "call of overloaded \[^\n\r\]* is ambiguous" }
}
