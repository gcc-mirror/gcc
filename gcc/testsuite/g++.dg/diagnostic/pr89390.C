// PR c++/89390
// { dg-do compile { target c++11 } }
// { dg-options "-fdiagnostics-show-caret" }

enum class bar { A, B, C };

void
foo ()
{
  bar::~bar ();    // { dg-error "8: '~bar' is not a member of 'bar'" }
  /* { dg-begin-multiline-output "" }
   bar::~bar ();
        ^~~~
     { dg-end-multiline-output "" } */
}

namespace ns { enum class baz { P, Q, R }; }

void
test_2 ()
{
  ns::baz::~baz ();    // { dg-error "12: '~ns::baz' is not a member of 'ns::baz'" }
  /* { dg-begin-multiline-output "" }
   ns::baz::~baz ();
            ^~~~
     { dg-end-multiline-output "" } */
}

struct first;
struct second;
second::~first() {} // { dg-error "9: declaration of '~first' as member of 'second'" }
  /* { dg-begin-multiline-output "" }
 second::~first() {}
         ^~~~~~
     { dg-end-multiline-output "" } */

struct test { ~test(); };
typedef test test_t;
~test_t();  // { dg-error "typedef-name 'test_t' used as destructor declarator" }
// { dg-error "expected" "" { target *-*-* } .-1 }
  /* { dg-begin-multiline-output "" }
 ~test_t();
 ^~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 ~test_t();
          ^
     { dg-end-multiline-output "" } */
