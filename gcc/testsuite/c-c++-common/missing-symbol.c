/* { dg-options "-fdiagnostics-show-caret" } */

extern int foo (void);
extern int bar (void);

int missing_close_paren_in_switch (int i)
{
  switch (i /* { dg-message "10: to match this '\\('" } */
    { /* { dg-error "5: expected '\\)' before '.' token" } */
  /* { dg-begin-multiline-output "" }
     {
     ^
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
   switch (i
          ^
     { dg-end-multiline-output "" } */

    case 0:
      return 5;
    default:
      return i;
    }
} /* { dg-error "1: expected" } */
  /* { dg-begin-multiline-output "" }
 }
 ^
     { dg-end-multiline-output "" } */

void missing_close_paren_in_if (void)
{
  if (foo () /* { dg-line start_of_if } */
      && bar () 
    { /* { dg-error "5: expected '\\)' before '.' token" } */
      /* { dg-begin-multiline-output "" }
     {
     ^
         { dg-end-multiline-output "" } */
      /* { dg-message "6: to match this '\\('" "" { target *-*-* } start_of_if } */
      /* { dg-begin-multiline-output "" }
   if (foo ()
      ^
      { dg-end-multiline-output "" } */
    }

} /* { dg-error "1: expected" } */
  /* { dg-begin-multiline-output "" }
 }
 ^
     { dg-end-multiline-output "" } */
