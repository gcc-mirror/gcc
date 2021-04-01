// PR c++/98481
// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-version=14 }
inline namespace N __attribute ((__abi_tag__ ("myabi")))
{
  struct A {};
}
template <typename T>
struct B { typedef int size_type; };
struct S1 { B<A>::size_type foo () const { return 1; } };
struct S2 { B<A>::size_type foo () const; };
int S2::foo () const { return 2; }
int (S1::*f1) () const = &S1::foo;
int (S2::*f2) () const = &S2::foo;

// { dg-final { scan-assembler-not "_ZNK2S13fooEv" } }
// { dg-final { scan-assembler "_ZNK2S23fooEv" } }
// { dg-final { scan-assembler "_ZNK2S13fooB5myabiEv" } }
