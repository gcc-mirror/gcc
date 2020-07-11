/* { dg-do compile } */
/* { dg-options "-O1" } */

struct a {
  int b;
  short c;
};
int d;
void e() {
  struct a f[1];
  f[d] = f[d];
}

struct S {
  int a[30];
  int c;
};

int get_int (void);

int foo(struct S p)
{
  p.c = get_int ();
  p.a[get_int()] = get_int()+1;
  return p.c;
}
