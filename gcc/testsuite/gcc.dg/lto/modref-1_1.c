short aa;
void
__attribute__ ((noinline, noclone))
recursive (int *a, int *b, int *c, int level)
{
  if (level && c)
    {
      recursive (b,a,c,0);
      aa++;
    }
  else
    *a=0;
}
