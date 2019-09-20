/* PR c/91815 */
/* { dg-do compile } */

int f (void)
{
  extern float v;   

  return (v > 0.0f);
}

extern int t;

typedef float t; /* { dg-error "redeclared as different kind of symbol" } */

t v = 4.5f;
