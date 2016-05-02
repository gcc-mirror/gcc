// { dg-options "-fdiagnostics-show-caret" }

union u
{
  int color;
  int shape;
};

int test (union u *ptr)
{
  return ptr->colour; // { dg-error "did you mean .color.?" }
}

// Verify that we get an underline and a fixit hint.
/* { dg-begin-multiline-output "" }
   return ptr->colour;
               ^~~~~~
               color
   { dg-end-multiline-output "" } */
