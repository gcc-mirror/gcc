/* PR c/91815 */
/* { dg-do compile } */

int f (void)
{
  extern float v;   
/* { dg-message "note: previous declaration" "previous declaration" { target *-*-* } .-1 } */
  return (v > 0.0f);
}

extern int t;
/* { dg-message "note: previous declaration" "previous declaration" { target *-*-* } .-1 } */

typedef float t; /* { dg-error "redeclared as different kind of symbol" } */

t v = 4.5f;  /* { dg-error "conflicting types" } */
