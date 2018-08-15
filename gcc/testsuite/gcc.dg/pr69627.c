/* PR c/69627 */
/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret" } */

void
foo ()
{
  float t[2] = { 1, 2 };
  int const *s = 0;
  t[1] / s;	/* { dg-error "invalid operands to binary /" } */
/* { dg-begin-multiline-output "" }
   t[1] / s;
   ~~~~ ^
    |
    float
   { dg-end-multiline-output "" } */
}

void
bar ()
{
  float t[2] = { 1, 2 };
  int const *s[2] = { 0, 0 };
  t[1] / s[0];	/* { dg-error "invalid operands to binary /" } */
/* { dg-begin-multiline-output "" }
   t[1] / s[0];
   ~~~~ ^ ~~~~
    |      |
    float  const int *
   { dg-end-multiline-output "" } */
}
