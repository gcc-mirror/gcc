/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret" } */

void test_1 (signed char e);

/* PR c/70339.  */
void test_2 (singed char e); // { dg-error "6: variable or field 'test_2' declared void" }
/* { dg-begin-multiline-output "" }
 void test_2 (singed char e);
      ^~~~~~
   { dg-end-multiline-output "" } */
// { dg-message "14: 'singed' was not declared in this scope; did you mean 'signed'\\?" "" { target *-*-* } 7 }
/* { dg-begin-multiline-output "" }
 void test_2 (singed char e);
              ^~~~~~
              signed
   { dg-end-multiline-output "" } */

void test_3 (car e); // { dg-error "6: variable or field 'test_3' declared void" }
/* { dg-begin-multiline-output "" }
 void test_3 (car e);
      ^~~~~~
   { dg-end-multiline-output "" } */
// { dg-message "14: 'car' was not declared in this scope; did you mean 'char'\\?" "" { target *-*-* } 19 }
/* { dg-begin-multiline-output "" }
 void test_3 (car e);
              ^~~
              char
   { dg-end-multiline-output "" } */

/* TODO: this one could be handled better.  */
void test_4 (signed car e); // { dg-error "25: expected ',' or '...' before 'e'" }
/* { dg-begin-multiline-output "" }
 void test_4 (signed car e);
                         ^
   { dg-end-multiline-output "" } */

/* Verify that we handle misspelled typedef names.  */

typedef struct something {} something_t;

some_thing_t test_5; // { dg-error "1: 'some_thing_t' does not name a type; did you mean 'something_t'?" }
  /* { dg-begin-multiline-output "" }
 some_thing_t test_5;
 ^~~~~~~~~~~~
 something_t
   { dg-end-multiline-output "" } */

/* TODO: we don't yet handle misspelled struct names.  */
struct some_thing test_6; // { dg-error "aggregate 'some_thing test_6' has incomplete type and cannot be defined" }
  /* { dg-begin-multiline-output "" }
 struct some_thing test_6;
                   ^~~~~~
   { dg-end-multiline-output "" } */

typedef long int64_t;
int64 i; // { dg-error "1: 'int64' does not name a type; did you mean 'int64_t'?" }
/* { dg-begin-multiline-output "" }
 int64 i;
 ^~~~~
 int64_t
   { dg-end-multiline-output "" } */

/* Verify that gcc doesn't offer nonsensical suggestions.  */

nonsensical_suggestion_t var; /* { dg-bogus "did you mean" } */
/* { dg-error "'nonsensical_suggestion_t' does not name a type" "" { target { *-*-* } } .-1 } */
/* { dg-begin-multiline-output "" }
 nonsensical_suggestion_t var;
 ^~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

singed char ch; // { dg-error "1: 'singed' does not name a type; did you mean 'signed'?" }
/* { dg-begin-multiline-output "" }
 singed char ch;
 ^~~~~~
 signed
   { dg-end-multiline-output "" } */
