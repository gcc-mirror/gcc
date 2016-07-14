/* PR c/71858 */
/* Make sure anticipated builtins are not considered before they are declared.  */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-function-declaration -fdiagnostics-show-caret" } */

int sscafn (const char *, const char *, ...);

int
test_1 (const char *p)
{
  int i;
  return ssacnf (p, "%d", &i); /* { dg-warning "10: implicit declaration of function .ssacnf.; did you mean .sscafn.?" } */
  /* { dg-begin-multiline-output "" }
   return ssacnf (p, "%d", &i);
          ^~~~~~
          sscafn
   { dg-end-multiline-output "" } */
}

int scafn (const char *, ...);
int scanf (const char *, ...);

int
test_2 (void)
{
  int i;
  return sacnf ("%d", &i); /* { dg-warning "10: implicit declaration of function .sacnf.; did you mean .scanf.?" } */
  /* { dg-begin-multiline-output "" }
   return sacnf ("%d", &i);
          ^~~~~
          scanf
   { dg-end-multiline-output "" } */
}
