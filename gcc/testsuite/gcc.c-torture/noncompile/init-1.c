struct a
{ double a, b, c; };

struct b
{
  struct a arr[6];
};

static struct b a_b =
{
  {0,0,0},
  {0,0,0},
  {0,0,0},
  {0,0,0},
  {0,0,0},
  {0,0,0},
};
