void abort(void);
void exit(int);

void
test1(void)
{
  int x = 3, y = 2;

  if ((x < y ? x++ : y++) != 2)
    abort ();

  if (x != 3)
    abort ();

  if (y != 3)
    abort ();
}

void
test2(void)
{
  int x = 3, y = 2, z;

  z = (x < y) ? x++ : y++;
  if (z != 2)
    abort ();

  if (x != 3)
    abort ();

  if (y != 3)
    abort ();
}

void
test3(void)
{
  int x = 3, y = 2;
  int xx = 3, yy = 2;

  if ((xx < yy ? x++ : y++) != 2)
    abort ();

  if (x != 3)
    abort ();

  if (y != 3)
    abort ();
}

int x, y;

static void
init_xy(void)
{
  x = 3;
  y = 2;
}

void
test4(void)
{
  init_xy();
  if ((x < y ? x++ : y++) != 2)
    abort ();

  if (x != 3)
    abort ();

  if (y != 3)
    abort ();
}

void
test5(void)
{
  int z;

  init_xy();
  z = (x < y) ? x++ : y++;
  if (z != 2)
    abort ();

  if (x != 3)
    abort ();

  if (y != 3)
    abort ();
}

void
test6(void)
{
  int xx = 3, yy = 2;
  int z;

  init_xy();
  z = (xx < y) ? x++ : y++;
  if (z != 2)
    abort ();

  if (x != 3)
    abort ();

  if (y != 3)
    abort ();
}

int
main(){
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  exit (0);
}
