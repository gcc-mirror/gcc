/* PR middle-end/8408 */
/* Verify that the recognizer explicitly
   handles ADDRESSOF operands. */

void foo(void)
{
  double d1 = 3.14159, d2;
  if (&d2 == &d1)
     ;
}
