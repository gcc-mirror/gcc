/* { dg-do compile } */
/* { dg-options "-Ofast" } */

enum b
{
  c,
  d
};
struct e
{
  enum b code;
};
struct f
{
  unsigned g;
};
int h, i;
struct a
{
  struct e common;
  struct f j;
};

struct a k (void)
{
  struct a *l;
  do
    if (l->common.code == d && l->j.g * 4)
      ;
    else
      i = l->j.g | (l->common.code && l);
  while (h && l->common.code == c);
}
