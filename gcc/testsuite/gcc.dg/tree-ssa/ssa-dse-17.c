/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1" } */

struct s1 {
    unsigned short f1;
    unsigned char f2;
};

struct s2 {
    struct s1 *p1;
};

void f1(struct s2 *p)
{
  p->p1->f2 = 9;
  p->p1->f2 = 10;
}

/* { dg-final { scan-tree-dump-times "f2 =" 1 "dse1" } } */
