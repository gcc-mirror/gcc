/* PR middle-end/111683 */
/* { dg-do run } */
/* { dg-options "-O2" } */

long long b[6] = { 3, 4, 5, 6, 7, 8 }, c[16];
long long d[9] = { 3, 7, 12, 18, 22, 26, 21, 15, 8 };
typedef long long U __attribute__ ((vector_size(16), may_alias, aligned(1)));
typedef long long V __attribute__ ((vector_size(16), may_alias));

int
main ()
{
  for (int f = 0; f < 6; f++)
    {
      *(U *) &c[f] = *(U *) &c[f] + (V) { b[f], b[f] };
      *(U *) &c[f + 2] = *(U *) &c[f + 2] + (V) { b[f], b[f] };
    }
  for (int f = 0; f < 9; f++)
    if (c[f] != d[f])
      __builtin_abort ();
  return 0;
}
