int a[8];
int s244(void)
{
  int lrc, j;
  lrc = 0;
  for (j=0; j<7; j++)
    if(a[j] != a[j+1])
      lrc = 1;
  if (lrc != 0)
    return 0;
  return 1;
}
