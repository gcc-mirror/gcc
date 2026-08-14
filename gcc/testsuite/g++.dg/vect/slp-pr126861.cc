// { dg-do compile }

struct Guard { ~Guard(); };
void init();

int f (unsigned n1, unsigned n2)
{
  Guard g;
  init();
  return (n1 != 0) + (n2 != 0);
}
