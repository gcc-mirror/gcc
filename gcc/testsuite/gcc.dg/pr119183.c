/* PR c/119183 */
/* { dg-do compile } */

int foo (void);
#define A(x) (1.0f * (1.0f * (1.0f * (1.0f * (1.0f * (1.0f * (1.0f * (1.0f * (x)))))))))

float
bar (float r)
{
  r += A (A (A (A (A (A (A (A (foo ()))))))));
  return r;
}
