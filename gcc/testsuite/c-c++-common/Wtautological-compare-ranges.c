/* { dg-do compile } */
/* { dg-options "-Wtautological-compare -fdiagnostics-show-caret" } */

#define FOO foo

void
fn1 (int foo)
{
  if (foo == foo); /* { dg-warning "self-comparison always evaluates to true" } */
  /* { dg-begin-multiline-output "" }
   if (foo == foo);
           ^~
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
   if (foo == foo);
       ~~~ ^~ ~~~
     { dg-end-multiline-output "" { target c++ } } */
}

void
fn2 (int foo)
{
  if (FOO == FOO); /* { dg-warning "self-comparison always evaluates to true" } */
  /* { dg-begin-multiline-output "" }
   if (FOO == FOO);
           ^~
     { dg-end-multiline-output "" } */
}

void
fn3 (int foo)
{
  if ((foo & 16) == 10); /* { dg-warning "bitwise comparison always evaluates to false" } */
  /* { dg-begin-multiline-output "" }
   if ((foo & 16) == 10);
                  ^~
     { dg-end-multiline-output "" { target c } } */
  /* { dg-begin-multiline-output "" }
   if ((foo & 16) == 10);
       ~~~~~~~~~~ ^~ ~~
     { dg-end-multiline-output "" { target c++ } } */
}
