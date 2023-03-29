// { dg-do compile { target { ! default_packed } } }
// Test that -Waddress-of-packed-member works with member functions.

struct S {
  char c;
} __attribute__((packed));

struct X {
  S* memfn ();
  static S* smemfn ();
} x;

S *foo ();

S**
f ()
{
  S **s;
  s = reinterpret_cast<S**>(foo ()); // { dg-warning "converting a packed" }
  s = reinterpret_cast<S**>(x.memfn ()); // { dg-warning "converting a packed" }
  s = reinterpret_cast<S**>(X::smemfn ()); // { dg-warning "converting a packed" }
  return s;
}
