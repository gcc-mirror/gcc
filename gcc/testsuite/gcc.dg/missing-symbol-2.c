/* { dg-options "-fdiagnostics-show-caret -Wno-switch-unreachable" } */

extern int foo (void);

void missing_open_paren (void)
{
  if foo ()) /* { dg-line missing_open_paren } */
    {
    }
  /* { dg-error "expected '\\(' before 'foo'" "" { target c } missing_open_paren } */
  /* { dg-begin-multiline-output "" }
   if foo ())
      ^~~
      (
     { dg-end-multiline-output "" } */
  /* { dg-error "expected statement before '\\)' token"  "" { target c } missing_open_paren } */
  /* { dg-begin-multiline-output "" }
   if foo ())
            ^
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


/* We don't offer a fix-it hint for this case in C, as it could be
   colon or ellipsis.
   TODO: we could be smarter about error-recovery here; given the
   return perhaps we could assume a missing colon.  */

int missing_colon_in_switch (int val)
{
  switch (val)
    {
    case 42
      return 42; /* { dg-error "expected ':' or '...' before 'return'" } */
    /* { dg-begin-multiline-output "" }
       return 42;
       ^~~~~~
       { dg-end-multiline-output "" } */

    default:
      return val;
    }
}

/* { dg-begin-multiline-output "" }
 int dummy;
 ^~~
   { dg-end-multiline-output "" } */
int dummy;/* { dg-error "expected declaration or statement at end of input" "" { target c } } */
