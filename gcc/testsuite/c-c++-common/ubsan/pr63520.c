/* PR sanitizer/63520 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int a;

void
foo (void)
{
  while (1)
    {
      if (a == 1)
	break;
      a -= 1;
    }
}
