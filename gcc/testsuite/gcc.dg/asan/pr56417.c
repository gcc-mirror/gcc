/* PR sanitizer/56417 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -w" } */

int
foo (void)
{
  return __builtin_strlen (&foo);
}
