/* Ensure that we get an error on the call to fn_a with an
   int arg below.  */
/* { dg-additional-options "-std=c23" } */

/* Verify that the first excess param is underlined.  */
/* { dg-additional-options "-fdiagnostics-show-caret" } */

extern void fn_a (); /* { dg-message "declared here" } */
extern void fn_b (void); /* { dg-message "declared here" } */
extern void fn_c (int); /* { dg-message "declared here" } */

void test_known_fn (void)
{
  fn_a (42); /* { dg-error "too many arguments to function 'fn_a'; expected 0, have 1" } */
  /* { dg-begin-multiline-output "" }
   fn_a (42);
   ^~~~  ~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 extern void fn_a ();
             ^~~~
     { dg-end-multiline-output "" } */

  fn_b (1776); /* { dg-error "too many arguments to function 'fn_b'; expected 0, have 1" } */
  /* { dg-begin-multiline-output "" }
   fn_b (1776);
   ^~~~  ~~~~
   { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 extern void fn_b (void);
             ^~~~
     { dg-end-multiline-output "" } */

  fn_c (1066, 1649);  /* { dg-error "too many arguments to function 'fn_c'; expected 1, have 2" } */
  /* { dg-begin-multiline-output "" }
   fn_c (1066, 1649);
   ^~~~        ~~~~
   { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
 extern void fn_c (int);
             ^~~~
     { dg-end-multiline-output "" } */
}

struct foo
{
  void (*callback_a)(); /* { dg-message "declared here" } */
  void (*callback_b)(void); /* { dg-message "declared here" } */
  void (*callback_c)(int); /* { dg-message "declared here" } */
};

void test_callback (struct foo *f)
{
  f->callback_a (42); /* { dg-error "too many arguments to function 'f->callback_a'; expected 0, have 1" } */
  /* { dg-begin-multiline-output "" }
   f->callback_a (42);
   ^              ~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   void (*callback_a)();
          ^~~~~~~~~~
     { dg-end-multiline-output "" } */
  
  f->callback_b (1776); /* { dg-error "too many arguments to function 'f->callback_b'; expected 0, have 1" } */
  /* { dg-begin-multiline-output "" }
   f->callback_b (1776);
   ^              ~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   void (*callback_b)(void);
          ^~~~~~~~~~
     { dg-end-multiline-output "" } */

  f->callback_c (1066, 1649); /* { dg-error "too many arguments to function 'f->callback_c'; expected 1, have 2" } */
  /* { dg-begin-multiline-output "" }
   f->callback_c (1066, 1649);
   ^                    ~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   void (*callback_c)(int);
          ^~~~~~~~~~
     { dg-end-multiline-output "" } */
}
