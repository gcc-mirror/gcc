/* PR c/91815 */
/* { dg-do compile } */

int f (void)
{
  extern int t;
  extern float v;   

  return (v > 0.0f);
}

typedef float t;

t v = 4.5f;
