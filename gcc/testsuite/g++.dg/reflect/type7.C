// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -fconcepts-diagnostics-depth=2" }

using info = decltype(^^int);

template<typename T> concept C = requires {
  typename [:T::r1:]; // { dg-error ".r1. is not a member of .int." }
  typename [:T::r2:]<int>;  // { dg-error ".r2. is not a member of .int." }
};

void
g ()
{
  C auto a = 42;  // { dg-error "does not satisfy placeholder constraints" }
}
