// PR c++/53932
// { dg-do link }

static union { int i; };
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
