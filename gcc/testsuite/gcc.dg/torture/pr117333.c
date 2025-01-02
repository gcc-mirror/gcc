/* { dg-do compile } */

struct s
{
  unsigned long ul;
  int i;
  char ac[];
};

const struct s gs = { 3, -4, "abcdef" };

void copy_s(struct s*d, const struct s*s)
{
  *d = *s;
}

unsigned test(struct s*ps, _Bool direct)
{
  if(direct)
    *ps = gs;
  else
    copy_s(ps, &gs);
  return sizeof(*ps);
}

unsigned size(void)
{
  return sizeof(gs);
}
