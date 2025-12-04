/* { dg-do link } */
/* { dg-options "-O2" } */

volatile int gi;

static const int values[25] = {5, 7, 11};

void link_error (void);

[[gnu::noinline]] void foo (int index)
{
  const int v = values[index];
  if (v <= -2
      || v > 11)
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
