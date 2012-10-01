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
    typename Foo<::B>::template Nested<::B> n; // { dg-error "17:'<::' cannot begin" "17-begin" { target c++98 } }
// { dg-message "17:'<:' is an alternate spelling" "17-alt" { target c++98 } 19 }
// { dg-error "39:'<::' cannot begin" "39-begin" { target c++98 } 19 }
// { dg-message "39:'<:' is an alternate spelling" "39-alt" { target c++98 } 19 }
    n.template Nested<B>::method();
    n.template Nested<::B>::method();  // { dg-error "22:'<::' cannot begin" "error" { target c++98 } }
// { dg-message "22:'<:' is an alternate" "note" { target c++98 } 24 }
    Nested<B>::method();
    Nested<::B>::method(); // { dg-error "11:'<::' cannot begin" "error" { target c++98 } }
// { dg-message "11:'<:' is an alternate" "note" { target c++98 } 27 }
  }
};

template <int N> struct Foo2 {};
template struct Foo2<::B>;  // { dg-error "21:'<::' cannot begin" "begin" { target c++98 } }
// { dg-message "21:'<:' is an alternate" "alt" { target c++98 } 33 }
// { dg-message "25:type/value mismatch" "mismatch" { target *-*-* } 33 }
// { dg-error "25:expected a constant" "const" { target *-*-* } 33 }

int value = 0;

void func(void)
{
  Foo<::B> f; // { dg-error "cannot begin" "begin" { target c++98 } }
// { dg-message "alternate spelling" "alt" { target c++98 } 42 }
  f.Foo<B>::method();
  f.Foo<::B>::method(); // { dg-error "8:cannot begin" "begin" { target c++98 } }
// { dg-message "8:alternate spelling" "alt" { target c++98 } 45 }

  // Check cases where we the token sequence is the correct one, but there
  //  was no digraph or whitespaces in the middle, so we should not emit
  //  the special error message.
  Foo<: :B> k2;     // { dg-bogus "cannot begin|alternate spelling" "smart error should not be triggered here" }
  Foo[:B> k1;       // { dg-bogus "cannot begin|alternate spelling" "smart error should not be triggered here" } 
// { dg-error "6:missing template arguments before" "template" { target *-*-* } { 51 } }
// { dg-error "9:expected primary-expression before ':' token" "primary" { target *-*-* } 51 }
// { dg-error "9:expected '\]' before ':' token" "backslash" { target *-*-* } 51 }
// { dg-error "9:expected ';' before ':' token" "semicolon" { target *-*-* } 51 }
// { dg-error "6:missing template arguments before" "template" { target *-*-* } 52 }
// { dg-error "7:expected primary-expression before ':' token" "primary" { target *-*-* } 52 }
// { dg-error "7:expected '\]' before ':' token" "backslash" { target *-*-* } 52 }
// { dg-error "7:expected ';' before ':' token" "semicolon" { target *-*-* } 52 }
//
  int Foo[2];
  Foo[::value] = 0;
}

template struct Foo<::B>; // { dg-error "20:'<::' cannot begin" "begin" { target c++98 } }
// { dg-message "20:is an alternate" "alt" { target c++98 } 66 }

// On the first error message, an additional note about the use of 
//  -fpermissive should be present
// { dg-message "17:\\(if you use '-fpermissive' or '-std=c\\+\\+11', or '-std=gnu\\+\\+11' G\\+\\+ will accept your code\\)" "-fpermissive" { target c++98 } 19 }
