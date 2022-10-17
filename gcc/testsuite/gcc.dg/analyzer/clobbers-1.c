#include "analyzer-decls.h"

struct foo
{
  int i;
  int j;
};

struct coord
{
  int x;
  int y;
  int z;
};

struct foo g;

void test_1 (void)
{
  g.i = 42;
  if (g.j)
    __analyzer_eval (g.j); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (g.j); /* { dg-warning "FALSE" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

void test_2 (struct foo f)
{
  f.i = 42;
  if (f.j)
    __analyzer_eval (f.j); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (f.j); /* { dg-warning "FALSE" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

void test_3 (struct foo *p)
{
  struct foo f = *p;
  f.i = 42;
  if (f.j)
    __analyzer_eval (f.j); /* { dg-warning "TRUE" } */
  else
    __analyzer_eval (f.j); /* { dg-warning "FALSE" } */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

void test_4 (struct coord *p)
{
  struct coord f = *p;
  f.x = 42;
  __analyzer_eval (f.y == p->y); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.z == p->z); /* { dg-warning "TRUE" } */
}

struct s5
{
  char arr[8];
};

void test_5 (struct s5 *p)
{
  struct s5 f = *p;
  f.arr[3] = 42;
  __analyzer_eval (f.arr[0] == p->arr[0]); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[1] == p->arr[1]); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[2] == p->arr[2]); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[3] == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[4] == p->arr[4]); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[5] == p->arr[5]); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[6] == p->arr[6]); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[7] == p->arr[7]); /* { dg-warning "TRUE" } */
}

struct s6
{
  int before; /* Give "arr" a nonzero offset.  */
  struct foo arr[4];
  int after;
};

void test_6 (struct s6 *p, struct foo *q)
{
  struct s6 f = *p;
  f.arr[1] = *q;
  __analyzer_eval (f.before == p->before); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[0].i == p->arr[0].i); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[0].j == p->arr[0].j); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[1].i == q->i); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[1].j == q->j); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[2].i == p->arr[2].i); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[2].j == p->arr[2].j); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[3].i == p->arr[3].i); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.arr[3].j == p->arr[3].j); /* { dg-warning "TRUE" } */
  __analyzer_eval (f.after == p->after); /* { dg-warning "TRUE" } */
}
