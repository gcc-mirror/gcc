/* { dg-do compile } */
/* { dg-options "-O" } */

struct i {
   int c;
};

static int
p(struct i a)
{
  return 0;
}

void
h(void)
{
  struct i z[] = {{ 0 }};
  int e[] = {};
  int x;
  e[0] = p(z[x]) + z[x].c;
}
