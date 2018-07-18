/* { dg-options "-fdiagnostics-show-caret" } */

union u
{
  int color;
  int shape;
};

int test (union u *ptr)
{
  return ptr->colour; /* { dg-error "did you mean .color.?" } */

/* Verify that we get an underline and a fixit hint.  */
/* { dg-begin-multiline-output "" }
   return ptr->colour;
               ^~~~~~
               color
   { dg-end-multiline-output "" } */
}


/* Verify that we don't offer a fixit hint in the presence of
   a macro.  */
int test_macro (union u *ptr)
{
#define FIELD colour /* { dg-error "did you mean .color.?" } */
  return ptr->FIELD;

/* { dg-begin-multiline-output "" }
 #define FIELD colour
               ^~~~~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   return ptr->FIELD;
               ^~~~~
   { dg-end-multiline-output "" } */

#undef FIELD
}
