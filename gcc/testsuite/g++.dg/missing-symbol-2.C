/* { dg-options "-fdiagnostics-show-caret" } */

extern int foo (void);

void missing_open_paren (void)
{
  if foo ()) /* { dg-error "expected '\\(' before 'foo'" } */
    {
    }
  /* { dg-begin-multiline-output "" }
   if foo ())
      ^~~
      (
     { dg-end-multiline-output "" } */
}


void missing_close_square (void)
{
  const char test [42;  /* { dg-error "22: expected ']' before ';' token" } */
  /* { dg-begin-multiline-output "" }
   const char test [42;
                      ^
                      ]
     { dg-end-multiline-output "" } */
}

int missing_semicolon (void)
{
  return 42 /* { dg-error "expected ';'" } */
}
/* { dg-begin-multiline-output "" }
   return 42
            ^
            ;
 }
 ~           
   { dg-end-multiline-output "" } */


int missing_colon_in_switch (int val)
{
  switch (val)
    {
    case 42 /* { dg-error "expected ':' before 'return'" } */
      return 42;
    /* { dg-begin-multiline-output "" }
     case 42
            ^
            :
       return 42;
       ~~~~~~
       { dg-end-multiline-output "" } */

    default:
      return val;
    }
}
