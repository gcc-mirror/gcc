/* PR c/83448 */

char *a;
int b;

void
foo (void)
{
  for (;;)
    {
      if (b < 0)
	foo ();
      __builtin_snprintf (a, b, "%*s", b, "");
    }
}
