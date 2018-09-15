/* PR middle-end/86864 */
/* Testcase by Serge Belyshev <belyshev@depni.sinp.msu.ru> */

long a;
void f ();
void g (int b, int c, int d)
{
  switch (b)
    {
    case 42:
    case 29:
    case 48:
    case 40:
    case 32:
      c = 2;
      break;
    case 0:
      c = 1;
      break;
    default:
      __builtin_unreachable ();
    }
  if (d || a)
    f ();
  if (c == 1)
    f ();
}
