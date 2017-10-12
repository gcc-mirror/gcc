/* { dg-options "-fdiagnostics-show-caret" } */

extern int foo (void);
extern int bar (void);

int missing_close_paren_in_switch (int i)
{
  switch (i /* { dg-error "12: expected '\\)' before '.' token" } */
    {
  /* { dg-begin-multiline-output "" }
   switch (i
          ~ ^
            )
     {
     ~       
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
      && bar () /* { dg-error "16: expected '\\)' before '.' token" } */
    {
      /* { dg-begin-multiline-output "" }
       && bar ()
                ^
                )
     {
     ~           
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

int missing_colon_in_ternary (int flag)
{
  return flag ? 42 0; /* { dg-error "expected ':' before numeric constant" } */
  /* { dg-begin-multiline-output "" }
   return flag ? 42 0;
                   ^~
                   :
     { dg-end-multiline-output "" } */
}
