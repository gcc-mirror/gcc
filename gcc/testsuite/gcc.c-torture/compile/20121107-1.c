/* PR middle-end/55219 */
/* Testcase by Markus Trippelsdorf <markus@trippelsdorf.de> */

int x, c, d, e, f, g, h, i;
double j;
const int k;
const enum { B } a;
void
fn1 (void)
{
  h = (g ? c : g ? f : g ? e : g ? i : g ? f : g ? e : g ? d : x)
      + (a ? : a ? : a ? : a ? : a ? : a ? : a ? : a ? : a ? : a ? : a
         ? j : a ? : 0 ? : a ? : a ? : a ? : a ? : a ? : a ? k : a ? : x);
}
