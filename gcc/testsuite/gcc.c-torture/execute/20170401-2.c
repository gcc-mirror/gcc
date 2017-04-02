void adjust_xy (short *, short *);

struct adjust_template
{
  short kx_x;
  short kx_y;
};

static struct adjust_template adjust = {1, 1};

main ()
{
  short x = 1, y = 1;

  adjust_xy (&x, &y);

  if (x != 2)
    abort ();

  exit (0);
}

void
adjust_xy (x, y)
     short  *x;
     short  *y;
{
  *x = adjust.kx_x * *x + adjust.kx_y * *y;
}
