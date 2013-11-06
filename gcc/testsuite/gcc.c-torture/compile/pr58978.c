/* PR tree-optimization/58978 */

int
foo (int x)
{
  switch (x)
    {
    case 0:
    case 1:
    case 9:
      break;
    default:
      __builtin_unreachable ();
    }
  return x;
}
