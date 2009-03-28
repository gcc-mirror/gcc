/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre" } */

struct f {
  float a;
  float b;
  float c;
  float d;
};

struct f a;

void h(float, float, float, float);

void g(void)
{
  float a1 = a.a, b = a.b, c = a.c, d = a.d;
  a.a = a1;
  a.b = b;
  a.c = c;
  a.d = d;
  h(a1, b, c, d);
}

/* { dg-final { scan-tree-dump-not "a\\\.? = " "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
