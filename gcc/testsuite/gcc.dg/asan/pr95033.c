/* PR sanitizer/95033 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=address" } */

struct a
{
  int b;
};

struct a c(_Complex d)
{
  return *(struct a *)&d;
}
