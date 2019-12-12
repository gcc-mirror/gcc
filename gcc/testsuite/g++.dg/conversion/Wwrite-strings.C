// { dg-options "-fdiagnostics-show-caret" }

/* Verify that -Wwrite-strings underlines the string literal in question.  */

extern int callee (const char *one, char *two, const char *three);

int test_1 ()
{
  return callee ("first", "second", "third"); // { dg-warning "string constant to 'char\\*'" }
  /* { dg-begin-multiline-output "" }
   return callee ("first", "second", "third");
                           ^~~~~~~~
     { dg-end-multiline-output "" } */
  // TODO: underline the pertinent param in the decl of callee
}

char *test_2 (void)
{
  return "foo"; // { dg-warning "string constant to 'char\\*'" }
  /* { dg-begin-multiline-output "" }
   return "foo";
          ^~~~~
     { dg-end-multiline-output "" } */
}
