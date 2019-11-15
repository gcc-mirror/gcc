/* Test C2x attribute syntax.  Invalid use of fallthrough attribute.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wextra" } */

[[fallthrough]]; /* { dg-error "'fallthrough' attribute at top level" } */

int [[fallthrough]] x; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

int g () [[fallthrough]]; /* { dg-warning "ignored" } */
/* { dg-message "that appertains to a type-specifier" "appertains" { target *-*-* } .-1 } */

int
f (int a)
{
  [[fallthrough]] int b = 2; /* { dg-warning "not followed by|ignored" } */
  switch (a)
    {
    case 1:
      b = 1; /* { dg-warning "may fall through" } */
    case 2:
      b = 2; /* { dg-warning "may fall through" } */
      [[fallthrough()]]; /* { dg-error "does not take any arguments" } */
    case 3:
      b += 7;
      break;
    case 4:
      b = 4; /* { dg-warning "may fall through" } */
      [[fallthrough(1)]]; /* { dg-error "does not take any arguments|expected" } */
    case 5:
      b += 5;
      break;
    }
  [[fallthrough]] return b; /* { dg-warning "ignored" } */
}
