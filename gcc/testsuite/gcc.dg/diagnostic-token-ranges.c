/* { dg-options "-fdiagnostics-show-caret -Wc++-compat -std=c11" } */

/* Verify that various diagnostics show source code ranges.  */

long double nanl (const char *);

/* These ones merely use token ranges; they don't use tree ranges.  */

void undeclared_identifier (void)
{
  name; /* { dg-error "'name' undeclared" } */
/*
{ dg-begin-multiline-output "" }
   name;
   ^~~~
{ dg-end-multiline-output "" }
*/
}

void unknown_type_name (void)
{
  foo bar; /* { dg-error "unknown type name 'foo'" } */
/*
{ dg-begin-multiline-output "" }
   foo bar;
   ^~~
{ dg-end-multiline-output "" }
*/

  qux *baz; /* { dg-error "unknown type name 'qux'" } */
/*
{ dg-begin-multiline-output "" }
   qux *baz;
   ^~~
{ dg-end-multiline-output "" }
*/
}

void test_identifier_conflicts_with_cplusplus (void)
{
  int new; /* { dg-warning "identifier 'new' conflicts with" } */
/*
{ dg-begin-multiline-output "" }
   int new;
       ^~~
{ dg-end-multiline-output "" }
*/
}

extern void
bogus_varargs (...); /* { dg-error "ISO C requires a named argument before '...'" } */
/*
{ dg-begin-multiline-output "" }
 bogus_varargs (...);
                ^~~
{ dg-end-multiline-output "" }
*/

extern void
foo (unknown_type param); /* { dg-error "unknown type name 'unknown_type'" } */
/*
{ dg-begin-multiline-output "" }
 foo (unknown_type param);
      ^~~~~~~~~~~~
{ dg-end-multiline-output "" }
*/

void wide_string_literal_in_asm (void)
{
  __asm (L"nop"); /* { dg-error "wide string literal in 'asm'" } */
/*
{ dg-begin-multiline-output "" }
   __asm (L"nop");
          ^~~~~~
{ dg-end-multiline-output "" }
*/
}

void break_and_continue_in_wrong_places (void)
{
  if (0)
    break; /* { dg-error "break statement not within loop or switch" } */
/* { dg-begin-multiline-output "" }
     break;
     ^~~~~
   { dg-end-multiline-output "" } */

  if (1)
    ;
  else
    continue; /* { dg-error "continue statement not within a loop" } */
/* { dg-begin-multiline-output "" }
     continue;
     ^~~~~~~~
    { dg-end-multiline-output "" } */
}

/* Various examples of bad type decls.  */

int float bogus; /* { dg-error "two or more data types in declaration specifiers" } */
/* { dg-begin-multiline-output "" }
 int float bogus;
     ^~~~~
    { dg-end-multiline-output "" } */

long long long bogus2; /* { dg-error "'long long long' is too long for GCC" } */
/* { dg-begin-multiline-output "" }
 long long long bogus2;
           ^~~~
    { dg-end-multiline-output "" } */

long short bogus3; /* { dg-error "both 'long' and 'short' in declaration specifiers" } */
/* { dg-begin-multiline-output "" }
 long short bogus3;
      ^~~~~
    { dg-end-multiline-output "" } */

signed unsigned bogus4; /* { dg-error "both 'signed' and 'unsigned' in declaration specifiers" } */
/* { dg-begin-multiline-output "" }
 signed unsigned bogus4;
        ^~~~~~~~
    { dg-end-multiline-output "" } */
