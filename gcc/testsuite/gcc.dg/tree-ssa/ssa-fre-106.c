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
    c.t.t = tmp;
  }
  struct s1 *t = &c.t.t;
  f(t->t, t->t1);
}

/* { dg-final { scan-tree-dump "Replaced \[^\r\n\]*.t1 with b" "fre1" } } */
/* { dg-final { scan-tree-dump "Replaced \[^\r\n\]*.t with a" "fre1" } } */
