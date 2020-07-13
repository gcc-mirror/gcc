/* PR ipa/96130 */
/* { dg-do compile } */

struct S { unsigned j : 3; };
int k, l, m;

void
foo (struct S x)
{
  while (l != 5)
    switch (x.j)
      {
      case 1:
      case 3:
      case 4:
      case 6:
      case 2:
      case 5:
	l = m;
      case 7:
      case 0:
	k = 0;
      default:
	break;
      }
}
