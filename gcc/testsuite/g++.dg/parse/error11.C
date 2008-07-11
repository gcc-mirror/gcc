// { dg-do compile }
// { dg-options "-fshow-column" }"
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
    typename Foo<::B>::template Nested<::B> n; // { dg-error "17: error: '<::' cannot begin|17: note: '<:' is an alternate spelling|39: error: '<::' cannot begin|39: note: '<:' is an alternate" }
    n.template Nested<B>::method();
    n.template Nested<::B>::method();  // { dg-error "22: error: '<::' cannot begin|22: note: '<:' is an alternate" }
    Nested<B>::method();
    Nested<::B>::method(); // { dg-error "11: error: '<::' cannot begin|11: note: '<:' is an alternate" }
  }
};

template <int N> struct Foo2 {};
template struct Foo2<::B>;  // { dg-error "21: error: '<::' cannot begin|21: note: '<:' is an alternate|25: error: type/value mismatch|25: error:   expected a constant" }

int value = 0;

void func(void)
{
  Foo<::B> f; // { dg-error "cannot begin|alternate spelling" }
  f.Foo<B>::method();
  f.Foo<::B>::method(); // { dg-error "8: error|8: note" }

  // Check cases where we the token sequence is the correct one, but there
  //  was no digraph or whitespaces in the middle, so we should not emit
  //  the special error message.
  Foo<: :B> k2;     // { dg-bogus "cannot begin|alternate spelling" "smart error should not be triggered here" }
  Foo[:B> k1;       // { dg-bogus "cannot begin|alternate spelling" "smart error should not be triggered here" } 
// { dg-error "6: error: missing template arguments before" "" { target *-*-* } { 41 } }
// { dg-error "9: error: expected primary-expression before ':' token" "" { target *-*-* } 41 }
// { dg-error "9: error: expected '\]' before ':' token" "" { target *-*-* } 41 }
// { dg-error "9: error: expected ';' before ':' token" "" { target *-*-* } 41 }
// { dg-error "6: error: missing template arguments before" "" { target *-*-* } 42 }
// { dg-error "7: error: expected primary-expression before ':' token" "" { target *-*-* } 42 }
// { dg-error "7: error: expected '\]' before ':' token" "" { target *-*-* } 42 }
// { dg-error "7: error: expected ';' before ':' token" "" { target *-*-* } 42 }
//
  int Foo[2];
  Foo[::value] = 0;
}

template struct Foo<::B>; // { dg-error "20: error: '<::' cannot begin|20: note: '<:' is an alternate" }

// On the first error message, an additional note about the use of 
//  -fpermissive should be present
// { dg-error "17: note: \\(if you use '-fpermissive' G\\+\\+ will accept your code\\)" "-fpermissive" { target *-*-* } 19 }
