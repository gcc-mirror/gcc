#include "analyzer-decls.h"

struct s1
{
  char a;
  char b;
  char c;
  char d;
};

struct s2
{
  char arr[4];
};

struct s3
{
  struct inner {
    char a;
    char b;
  } arr[2];
};

void test_1 ()
{
  struct s1 x = {'A', 'B', 'C', 'D'};
  __analyzer_eval (x.a == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.b == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.c == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.d == 'D'); /* { dg-warning "TRUE" } */
  __analyzer_eval (((struct s2 *)&x)->arr[0] == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (((struct s2 *)&x)->arr[1] == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (((struct s2 *)&x)->arr[2] == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (((struct s2 *)&x)->arr[3] == 'D'); /* { dg-warning "TRUE" } */
  struct s3 *p3 = (struct s3 *)&x;
  __analyzer_eval (p3->arr[0].a == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p3->arr[0].b == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p3->arr[1].a == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p3->arr[1].b == 'D'); /* { dg-warning "TRUE" } */

  ((struct s2 *)&x)->arr[1] = '#';
  __analyzer_eval (((struct s2 *)&x)->arr[1] == '#'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.b == '#'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p3->arr[0].b == '#'); /* { dg-warning "TRUE" } */
}

void test_2 ()
{
  struct s2 x = {{'A', 'B', 'C', 'D'}};
  __analyzer_eval (x.arr[0] == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[1] == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[2] == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[3] == 'D'); /* { dg-warning "TRUE" } */
  struct s1 *p = (struct s1 *)&x;
  __analyzer_eval (p->a == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p->b == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p->c == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p->d == 'D'); /* { dg-warning "TRUE" } */
}

void test_3 ()
{
  struct s3 x = {'A', 'B', 'C', 'D'};
  __analyzer_eval (x.arr[0].a == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[0].b == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[1].a == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[1].b == 'D'); /* { dg-warning "TRUE" } */
  struct s1 *p1 = (struct s1 *)&x;
  __analyzer_eval (p1->a == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p1->b == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p1->c == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p1->d == 'D'); /* { dg-warning "TRUE" } */
  struct s2 *p2 = (struct s2 *)&x;
  __analyzer_eval (p2->arr[0] == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p2->arr[1] == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p2->arr[2] == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (p2->arr[3] == 'D'); /* { dg-warning "TRUE" } */
}
