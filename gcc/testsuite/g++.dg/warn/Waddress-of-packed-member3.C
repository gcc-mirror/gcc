// { dg-do compile { target { ! default_packed } } }
// { dg-additional-options -Wcast-align=strict }

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
  s = reinterpret_cast<S**>(foo ()); // { dg-warning "increases required alignment" }
  s = reinterpret_cast<S**>(x.memfn ()); // { dg-warning "increases required alignment" }
  s = reinterpret_cast<S**>(X::smemfn ()); // { dg-warning "increases required alignment" }
  return s;
}
