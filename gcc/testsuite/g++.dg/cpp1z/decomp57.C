// PR c++/108474
// { dg-do link { target c++17 } }

struct T { int i, j; };
T h;
auto [i, j] = h;
int &r = i;
int s = i;
int *t = &i;

void
foo (int **p, int *q)
{
  static int &u = i;
  static int v = i;
  static int *w = &i;
  int &x = i;
  int y = i;
  int *z = &i;
  *p = &i;
  *q = i;
}

int
main ()
{
}
