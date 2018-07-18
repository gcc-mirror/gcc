/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret" } */

void test_1 (signed char e);

/* PR c/70339.  */
void test_2 (singed char e); /* { dg-error "14: unknown type name .singed.; did you mean .signed.?" } */
/* { dg-begin-multiline-output "" }
 void test_2 (singed char e);
              ^~~~~~
              signed
   { dg-end-multiline-output "" } */

void test_3 (car e); /* { dg-error "14: unknown type name .car.; did you mean .char.?" } */
/* { dg-begin-multiline-output "" }
 void test_3 (car e);
              ^~~
              char
   { dg-end-multiline-output "" } */

/* TODO: this one could be handled better.  */
void test_4 (signed car e); /* { dg-error "25: before .e." } */
/* { dg-begin-multiline-output "" }
 void test_4 (signed car e);
                         ^
   { dg-end-multiline-output "" } */

/* Verify that we handle misspelled typedef names.  */

typedef struct something {} something_t;

some_thing_t test_5; /* { dg-error "1: unknown type name .some_thing_t.; did you mean .something_t.?" } */
  /* { dg-begin-multiline-output "" }
 some_thing_t test_5;
 ^~~~~~~~~~~~
 something_t
   { dg-end-multiline-output "" } */

/* TODO: we don't yet handle misspelled struct names.  */
struct some_thing test_6; /* { dg-error "storage size of .test_6. isn't known" } */
  /* { dg-begin-multiline-output "" }
 struct some_thing test_6;
                   ^~~~~~
   { dg-end-multiline-output "" } */

typedef long int64_t;
int64 i; /* { dg-error "unknown type name 'int64'; did you mean 'int64_t'?" } */
/* { dg-begin-multiline-output "" }
 int64 i;
 ^~~~~
 int64_t
   { dg-end-multiline-output "" } */

/* Verify that gcc doesn't offer nonsensical suggestions.  */

nonsensical_suggestion_t var; /* { dg-bogus "did you mean" } */
/* { dg-error "unknown type name" "" { target { *-*-* } } .-1 } */
/* { dg-begin-multiline-output "" }
 nonsensical_suggestion_t var;
 ^~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */


/* In the following, we should suggest inserting "struct" (rather
   than "did you mean 'float'") and provide a fixit hint.  */
struct foo_t {
  int i;
};
foo_t *foo_ptr; /* { dg-error "1: unknown type name .foo_t.; use .struct. keyword to refer to the type" } */
/* { dg-begin-multiline-output "" }
 foo_t *foo_ptr;
 ^~~~~
 struct
   { dg-end-multiline-output "" } */


/* Similarly for unions.  */
union bar_t {
  int i;
  char j;
};
bar_t *bar_ptr; /* { dg-error "1: unknown type name .bar_t.; use .union. keyword to refer to the type" } */
/* { dg-begin-multiline-output "" }
 bar_t *bar_ptr;
 ^~~~~
 union
   { dg-end-multiline-output "" } */


/* Similarly for enums.  */
enum baz {
  BAZ_FIRST,
  BAZ_SECOND
};
baz value; /* { dg-error "1: unknown type name .baz.; use .enum. keyword to refer to the type" } */
/* { dg-begin-multiline-output "" }
 baz value;
 ^~~
 enum
   { dg-end-multiline-output "" } */

/* TODO: it would be better to detect the "singed" vs "signed" typo here.  */
singed char ch; /* { dg-error "7: before .char." } */
/* { dg-begin-multiline-output "" }
 singed char ch;
       ^~~~~
       ;
   { dg-end-multiline-output "" } */
