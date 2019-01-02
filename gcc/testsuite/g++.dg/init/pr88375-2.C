// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

enum struct a : int {
  one, two
};

constexpr int fn () { return 42; }

struct foo {
  int e1, e2;
  a e3;
} arr[] = {
  { 3, a::two }, // { dg-error "11: cannot convert 'a' to 'int' in initialization" }
  /* { dg-begin-multiline-output "" }
   { 3, a::two },
        ~~~^~~
           |
           a
     { dg-end-multiline-output "" } */
  { 6, 7, fn() }, // { dg-error "13: cannot convert 'int' to 'a' in initialization" }
  /* { dg-begin-multiline-output "" }
   { 6, 7, fn() },
           ~~^~
             |
             int
     { dg-end-multiline-output "" } */
};

struct bar {
  const char *f1;
  int f2;
} arr_2[] = {
  { 42 }, // { dg-error "5: invalid conversion from 'int' to 'const char\\*'" }
  /* { dg-begin-multiline-output "" }
   { 42 },
     ^~
     |
     int
     { dg-end-multiline-output "" } */
};
