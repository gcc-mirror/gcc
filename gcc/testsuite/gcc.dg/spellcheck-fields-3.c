/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret -std=c99" } */

/* Tests of incorrect name initializers.
   Verify that we get underlines and, where appropriate, fixit hints.  */

struct foo
{
  int foo;
  int bar;
};

union u
{
  int color;
  int shape;
};

/* Old-style named initializers.  */

struct foo old_style_f = {
 foa: 1, /* { dg-error ".struct foo. has no member named .foa.; did you mean .foo." } */
/* { dg-begin-multiline-output "" }
  foa: 1,
  ^~~
  foo
   { dg-end-multiline-output "" } */

 this_does_not_match: 3 /* { dg-error ".struct foo. has no member named .this_does_not_match." } */

/* { dg-begin-multiline-output "" }
  this_does_not_match: 3
  ^~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
};

union u old_style_u = { colour: 3 }; /* { dg-error ".union u. has no member named .colour.; did you mean .color.?" } */
/* { dg-begin-multiline-output "" }
 union u old_style_u = { colour: 3 };
                         ^~~~~~
                         color
   { dg-end-multiline-output "" } */

/* C99-style named initializers.  */

struct foo c99_style_f = {
  .foa = 1, /* { dg-error ".struct foo. has no member named .foa.; did you mean .foo." } */
/* { dg-begin-multiline-output "" }
   .foa = 1,
    ^~~
    foo
   { dg-end-multiline-output "" } */

  .this_does_not_match = 3 /* { dg-error ".struct foo. has no member named .this_does_not_match." } */
/* { dg-begin-multiline-output "" }
   .this_does_not_match = 3
    ^~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
};

union u c99_style_u = { .colour=3 }; /* { dg-error ".union u. has no member named .colour.; did you mean .color.?" } */
/* { dg-begin-multiline-output "" }
 union u c99_style_u = { .colour=3 };
                          ^~~~~~
                          color
   { dg-end-multiline-output "" } */
