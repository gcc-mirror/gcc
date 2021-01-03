// PR c++/25814
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdiagnostics-show-caret" }
// Test -Wvexing-parse's fix-it hints in C++11.

#include <initializer_list>

struct X { };

struct S {
  S(X);
  S(std::initializer_list<X>);
  int m;
};

struct T {
  T(X);
  int m;
};

struct W {
  W();
  W(std::initializer_list<X>);
  int m;
};

struct U {
  U();
  int m;
};

int
main ()
{
  /*
     Careful what we're suggesting:
     S a((X())) -> S(X)
     S a({X()}) -> (std::initializer_list<X>)
     S a{X()} -> (std::initializer_list<X>)
   */
  S a(X()); // { dg-warning "6:parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   S a(X());
      ^~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "6:add parentheses to declare a variable" "" { target *-*-* } 41 }
  /* { dg-begin-multiline-output "" }
   S a(X());
      ^~~~~
       (  )
     { dg-end-multiline-output "" } */

  T t(X()); // { dg-warning "6:parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   T t(X());
      ^~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "6:replace parentheses with braces to declare a variable" "" { target *-*-* } 53 }
  /* { dg-begin-multiline-output "" }
   T t(X());
      ^~~~~
      -
      {   -
          }
     { dg-end-multiline-output "" } */

  int n(   ); // { dg-warning "8:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   int n(   );
        ^~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "8:remove parentheses to default-initialize a variable" "" { target *-*-* } 67 }
  /* { dg-begin-multiline-output "" }
   int n(   );
        ^~~~~
        -----
     { dg-end-multiline-output "" } */
  // { dg-message "8:or replace parentheses with braces to value-initialize a variable" "" { target *-*-* } 67 }

  S s(); // { dg-warning "6:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   S s();
      ^~
     { dg-end-multiline-output "" } */

  X x(); // { dg-warning "6:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   X x();
      ^~
     { dg-end-multiline-output "" } */
  // { dg-message "6:remove parentheses to default-initialize a variable" "" { target *-*-* } 86 }
  /* { dg-begin-multiline-output "" }
   X x();
      ^~
      --
     { dg-end-multiline-output "" } */
  // { dg-message "6:or replace parentheses with braces to aggregate-initialize a variable" "" { target *-*-* } 86 }

  W w(); // { dg-warning "6:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   W w();
      ^~
     { dg-end-multiline-output "" } */
  // { dg-message "6:remove parentheses to default-initialize a variable" "" { target *-*-* } 99 }
  /* { dg-begin-multiline-output "" }
   W w();
      ^~
      --
     { dg-end-multiline-output "" } */

  T t2(); // { dg-warning "7:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   T t2();
       ^~
     { dg-end-multiline-output "" } */

  U u(); // { dg-warning "6:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   U u();
      ^~
     { dg-end-multiline-output "" } */
  // { dg-message "6:remove parentheses to default-initialize a variable" "" { target *-*-* } 117 }
  /* { dg-begin-multiline-output "" }
   U u();
      ^~
      --
     { dg-end-multiline-output "" } */
  // { dg-message "6:or replace parentheses with braces to value-initialize a variable" "" { target *-*-* } 117 }
}
