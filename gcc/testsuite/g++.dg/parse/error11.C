// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// Try to find out when the digraph '<:' is used as a mistake, and parse it
//  correctly to avoid cascaded errors.

struct B;

template <class A>
struct Foo 
{
  template <class T>
  struct Nested
  {
    static void method(void) {}
  };

  void method(void) {
    typename Foo<::B>::template Nested<::B> n; // { dg-error "cannot begin|alternate spelling" }
    n.template Nested<B>::method();
    n.template Nested<::B>::method();  // { dg-error "cannot begin|alternate spelling" }
    Nested<B>::method();
    Nested<::B>::method(); // { dg-error "cannot begin|alternate spelling" }
  }
};

template <int N> struct Foo2 {};
template struct Foo2<::B>;  // { dg-error "cannot begin|alternate spelling|type/value mismatch|expected a constant" }

int value = 0;

void func(void)
{
  Foo<::B> f; // { dg-error "cannot begin|alternate spelling" }
  f.Foo<B>::method();
  f.Foo<::B>::method(); // { dg-error "cannot begin|alternate spelling" }

  // Check cases where we the token sequence is the correct one, but there
  //  was no digraph or whitespaces in the middle, so we should not emit
  //  the special error message.
  Foo<: :B> k2;     // { dg-bogus "cannot begin|alternate spelling" "smart error should not be triggered here" }
  Foo[:B> k1;       // { dg-bogus "cannot begin|alternate spelling" "smart error should not be triggered here" } 
// { dg-error "" "" { target *-*-* } 40 }
// { dg-error "" "" { target *-*-* } 41 }

  int Foo[2];
  Foo[::value] = 0;
}

template struct Foo<::B>; // { dg-error "cannot begin|alternate spelling" }

// On the first error message, an additional note about the use of 
//  -fpermissive should be present
// { dg-error "-fpermissive" "-fpermissive" { target *-*-* } 18 }
