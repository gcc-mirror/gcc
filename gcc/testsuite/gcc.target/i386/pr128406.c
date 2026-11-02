/* PR tree-optimization/128406 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

typedef float v8sf __attribute__ ((vector_size (32)));
extern int rt (void) __attribute__ ((returns_twice));
extern int get (void);
extern void use (float, float, float, float, float, int);

/* x and a0...a3 occur in abnormal PHIs.  BB SLP changes the CONSTRUCTOR
   into a CONSTRUCTOR of vectors of a0...a3 and of x.  A splat of x at the
   CONSTRUCTOR would be a use of x after its redefinition.  */

void
f (v8sf *out, v8sf v, float x)
{
  get ();
  float a0 = v[0], a1 = v[1], a2 = v[2], a3 = v[3];
  int k = get ();
  float b0 = a0, b1 = a1, b2 = a2, b3 = a3;
  float y0 = x, y1 = x, y2 = x, y3 = x;
  x++;
  *out = (v8sf) { b0, b1, b2, b3, y0, y1, y2, y3 };
  get ();
  rt ();
  use (x, a0, a1, a2, a3, k);
}
