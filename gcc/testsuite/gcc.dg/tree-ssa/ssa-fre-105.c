/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre1-details" } */

struct s1
{
  int t, t1;
};

struct s3
{
  struct s1 t;
};

struct s2
{
  struct s3 t;
};

void f(int, int);
void l();
void g(int a, int b, int *p)
{
  struct s2 c;
  {
    struct s1 tmp = {a,b};
    struct s3 *t = &c.t;
    t->t = tmp;
  }
  f(c.t.t.t, c.t.t.t1);
}

/* { dg-final { scan-tree-dump "Replaced c.t.t.t1 with b" "fre1" } } */
/* { dg-final { scan-tree-dump "Replaced c.t.t.t with a" "fre1" } } */
