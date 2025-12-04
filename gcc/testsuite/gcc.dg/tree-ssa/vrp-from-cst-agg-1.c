/* { dg-do link } */
/* { dg-options "-O2" } */

int ga = 1;
int gb = 2;
int gc = 3;
volatile int gi;

static const int *vars[3] = {&ga, &gb, &gc};

void link_error (void);

[[gnu::noinline]] void foo (int index)
{
  const int *p = vars[index];
  if (!p)
    link_error ();
  else
    gi = *p;
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
