/* PR sanitizer/88426 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

int
foo (void)
{
  const float v = 0.0f;
  return (int) (v < 0.0f ? v : 0.0f);
}
