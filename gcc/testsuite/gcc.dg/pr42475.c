/* PR rtl-optimization/42475 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct { float x, y; } B;
typedef struct { float z; } C;
typedef struct { B b; C c; } D;

B
foo (float x, float y)
{
  B b = { .x = x, .y = y };
  return b;
}

B
bar (B b, B y)
{
  return foo (y.x + b.x, b.y);
}

B
baz (D p)
{
  D d = { };
  B y = bar (foo (0, (p.c.z) / 2), d.b);
  return y;
}
