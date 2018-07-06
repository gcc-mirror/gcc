/* PR tree-optimization/84740 */

void
frobulate_for_gcc (unsigned int v)
{
  const char *s;
  switch (v)
    {
    case 0:
      s = "foo";
      break;
    case 1:
      s = "bar";
      break;
    case 2:
      s = "spam";
      break;
    default:
      s = (const char *) 0;
      break;
    }
  if (!s)
    __builtin_printf ("%s\n", s);
}
