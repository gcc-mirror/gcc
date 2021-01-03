/* { dg-do compile } */

struct a {
  enum { b, c } d;
  unsigned e;
  unsigned f;
};
void j(struct a *a, int i, int h)
{
  unsigned f = a->f;
  switch (a->d)
    while (1)
      {
        if (i)
          {
    case b:
            if (h)
              goto k;
          }
        else
          f = 0;
    case c:;
      }
k:
  a->e = a->f = f;
}
