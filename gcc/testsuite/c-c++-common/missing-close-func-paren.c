/* { dg-options "-fdiagnostics-show-caret" } */

/* Verify that the C/C++ frontends show the pertinent opening symbol when
   a closing symbol is missing for a function call.  */

/* Verify that, when they are on the same line, that the opening symbol is
   shown as a secondary range within the main diagnostic.  */

extern int __attribute__((const)) foo (int a, int b, int c);

void single_func ()
{
  int single =
    foo (1, (1 + 2), (1 + 2 + 3):); /* { dg-error "expected '\\)' before ':' token" } */
  /* { dg-begin-multiline-output "" }
     foo (1, (1 + 2), (1 + 2 + 3):);
         ~                       ^
                                 )
     { dg-end-multiline-output "" } */
}

/* Verify that, when they are on different lines, that the opening symbol is
   shown via a secondary diagnostic.  */

void multi_func ()
{
  int multi =
    foo (1, /* { dg-message "to match this '\\('" } */
         (1 + 2),
         (1 + 2 + 3):); /* { dg-error "expected '\\)' before ':' token" } */
  /* { dg-begin-multiline-output "" }
          (1 + 2 + 3):);
                     ^
                     )
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
     foo (1,
         ^
     { dg-end-multiline-output "" } */
}
