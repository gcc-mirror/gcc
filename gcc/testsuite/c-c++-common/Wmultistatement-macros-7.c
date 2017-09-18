/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define SWAP(X, Y)      \
  tmp = X; /* { dg-warning "macro expands to multiple statements" } */ \
  X = Y;                \
  Y = tmp

#define BODY_AND_IF(COND, X, Y)  \
  if (COND) SWAP (X, Y) /* { dg-message "in expansion of macro .SWAP." } */

void
fn (int x, int y)
{
  int tmp;
  BODY_AND_IF (1, x, y); /* { dg-message "in expansion of macro .BODY_AND_IF." } */
}
