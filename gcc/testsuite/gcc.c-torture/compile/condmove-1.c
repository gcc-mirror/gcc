/* PR middle-end/111260 */

/* Used to ICE while expansion of the `(a == b) ? b : 0;` */
int f1(long long a)
{
  int b = 822920;
  int t = a == b;
  return t * (int)b;
}
