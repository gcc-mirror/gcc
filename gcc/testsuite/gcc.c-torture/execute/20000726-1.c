void abort (void);
void exit (int);

void adjust_xy (short *, short *);

struct adjust_template
{
  short kx_x;
  short kx_y;
  short kx;
  short kz;
};

static struct adjust_template adjust = {0, 0, 1, 1};

int
main (void)
{
  short x = 1, y = 1;

  adjust_xy (&x, &y);

  if (x != 1)
    abort ();

  exit (0);
}

void
adjust_xy (x, y)
     short  *x;
     short  *y;
{
  *x = adjust.kx_x * *x + adjust.kx_y * *y + adjust.kx;
}
