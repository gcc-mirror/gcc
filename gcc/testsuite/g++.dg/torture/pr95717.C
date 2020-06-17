// { dg-do compile }

bool a;
extern bool b[];
long c, d;
int *f;
void g(bool h)
{
  for (short e = 0; e < c; e = 4)
    for (; d; d++)
      b[d] = a = f[d] ? c ? h : 0 : h;
}
