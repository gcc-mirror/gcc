/* PR middle-end/14470 */
/* Origin: Lodewijk Voge <lvoge@cs.vu.nl> */

extern void abort(void);

int main()
{
  int t[1025] = { 1024 }, d;

  d = 0;
  d = t[d]++;
  if (t[0] != 1025)
    abort();
  if (d != 1024)
    abort();
  return 0;
}
