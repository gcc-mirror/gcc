void abort(void);
int x, y;
void init_xy(void);
void
test4(void)
{
  init_xy();
  _Bool iftemp0;
  int x1 = x;
  _Bool iftemp1;
  x1++;
  if (x1 != 3)
    {
      iftemp1 = 1;
      goto endfirstif;
    }
  iftemp1 = 0;
  endfirstif:
  iftemp0 = iftemp1;
  if (iftemp0)
    abort();
}

