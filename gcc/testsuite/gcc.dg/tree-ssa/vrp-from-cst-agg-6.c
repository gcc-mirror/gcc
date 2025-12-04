/* { dg-do run } */
/* { dg-options "-O2" } */

volatile int gi;

static const struct {
    int a;
    int b;
} values[2][2] = {
  { {0, 1 }, {0, 2} },
  { {0, 1 }, {0, 2} }
};

[[gnu::noipa]] static void
check (int v)
{
  if (!v)
    __builtin_abort ();
  gi = v;
}


[[gnu::noinline]] void foo (int i, int j)
{
  const int v = values[i][j].a;
  if (v <= 0
      || v > 2)
    return;
  check (v);
}

[[gnu::noipa]] int
get_index (void)
{
  return 1;
}

int main (int argc, char **argv)
{
  foo (get_index (), get_index ());
  return 0;
}
