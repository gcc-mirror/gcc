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

  ((struct s2 *)&x)->arr[1] = '#';
  __analyzer_eval (((struct s2 *)&x)->arr[1] == '#'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.b == '#'); /* { dg-warning "TRUE" } */
}

void test_2 ()
{
  struct s2 x = {{'A', 'B', 'C', 'D'}};
  __analyzer_eval (x.arr[0] == 'A'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[1] == 'B'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[2] == 'C'); /* { dg-warning "TRUE" } */
  __analyzer_eval (x.arr[3] == 'D'); /* { dg-warning "TRUE" } */
  struct s1 *p = (struct s1 *)&x;
  __analyzer_eval (p->a == 'A'); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  __analyzer_eval (p->b == 'B'); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  __analyzer_eval (p->c == 'C'); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  __analyzer_eval (p->d == 'D'); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
}
