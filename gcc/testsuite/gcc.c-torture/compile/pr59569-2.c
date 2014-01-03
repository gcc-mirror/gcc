/* PR middle-end/59569 */
void foo (int *a, int b)
{
  for (; b; b--)
    a[b] = 1;
}
