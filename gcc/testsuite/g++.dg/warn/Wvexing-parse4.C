// PR c++/25814
// { dg-do compile { target c++98_only } }
// { dg-additional-options "-fdiagnostics-show-caret" }
// Test -Wvexing-parse's fix-it hints in C++98.

struct X { };

struct T {
  T(X);
  int m;
};

struct U {
  U();
  int m;
};

int
main ()
{
  T t(X()); // { dg-warning "6:parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   T t(X());
      ^~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "6:add parentheses to declare a variable" "" { target *-*-* } 21 }
  /* { dg-begin-multiline-output "" }
   T t(X());
      ^~~~~
       (  )
     { dg-end-multiline-output "" } */

  int n(   ); // { dg-warning "8:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   int n(   );
        ^~~~~
     { dg-end-multiline-output "" } */
  // { dg-message "8:remove parentheses to default-initialize a variable" "" { target *-*-* } 33 }
  /* { dg-begin-multiline-output "" }
   int n(   );
        ^~~~~
        -----
     { dg-end-multiline-output "" } */

  T y(); // { dg-warning "6:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   T y();
      ^~
     { dg-end-multiline-output "" } */

  X x(); // { dg-warning "6:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   X x();
      ^~
     { dg-end-multiline-output "" } */
  // { dg-message "6:remove parentheses to default-initialize a variable" "" { target *-*-* } 51 }
  /* { dg-begin-multiline-output "" }
   X x();
      ^~
      --
     { dg-end-multiline-output "" } */

  U u(); // { dg-warning "6:empty parentheses were disambiguated as a function declaration" }
  /* { dg-begin-multiline-output "" }
   U u();
      ^~
     { dg-end-multiline-output "" } */
  // { dg-message "6:remove parentheses to default-initialize a variable" "" { target *-*-* } 63 }
  /* { dg-begin-multiline-output "" }
   U u();
      ^~
      --
     { dg-end-multiline-output "" } */
}
