/* { dg-do compile } */

/* There was an ICE due to diving by zero in the objc front-end. */

struct f
{
  int i;
  struct{} g[4];
  int tt;
};

char *e = @encode(struct f);
