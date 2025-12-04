/* { dg-do link } */
/* { dg-options "-O2" } */

volatile int gi;

static const struct {
    int a;
    int b;
} values[2][2] = {
  { {1000, 1 }, {1001, 2} },
  { {1003, 1 }, {1004, 2} }
};

void link_error (void);

[[gnu::noinline]] void foo (int i)
{
  const int v = values[0][i].b;
  if (v <= 0
      || v > 2)
    link_error ();
  else
    gi = v;
}

[[gnu::noipa]] int
get_index (void)
{
  return 1;
}

int main (int argc, char **argv)
{
  foo (get_index ());
  return 0;
}
