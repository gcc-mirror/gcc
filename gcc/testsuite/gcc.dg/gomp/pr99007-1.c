/* PR middle-end/99007 */

void
bar (int n)
{
  int i;
  long s[n];
  for (i = 0; i < n; i++)
    s[i] = 0;
  #pragma omp teams distribute parallel for reduction(+:s)
  for (i = 0; i < 8; i++)
    s[3]++;
}
