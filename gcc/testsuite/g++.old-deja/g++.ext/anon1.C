// { dg-do run  }
// { dg-options "" }

union U {
  struct { int i; int j; };
  int a[2];
};

int main ()
{
  U u;
  u.i = 42;
  u.a[1] = 24;
  return u.j != 24 || u.a[0] != 42;
}
