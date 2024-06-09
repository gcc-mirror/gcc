// PR c++/12909
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-require-weak "" }
// { dg-require-alias "" }
// { dg-options "-mavx -Wabi -fabi-version=2 -fabi-compat-version=0" }
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_Z1fIDv4_fEvT_" } }
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_Z1fIU8__vectorfEvT_" } }
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_ZN1AIDv4_fE1tE" } }
// { dg-final { scan-assembler "(weak|glob)\[^\n\]*_ZN1AIU8__vectorfE1tE" } }

#include <x86intrin.h>

template <class T>
struct A
{
  static T t;
};

template <class T>
T A<T>::t;			// { dg-warning "mangled name" }

template <class T>
void f (T t) { }		// { dg-warning "mangled name" }

int main()
{
  f (A<__v4sf>::t);
}
