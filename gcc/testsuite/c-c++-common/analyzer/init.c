/* Tests of brace-enclosed initializers
   Some of these use the CONSTRUCTOR tree code, but it appears
   only for a full zero-init; it appears that by the time the analyzer
   runs that this initialization has been converted into field-wise
   gimple assign stmts, with just "zero-init everything" CONSTRUCTORs
   and "clobber" CONSTRUCTORs.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct coord
{
  int x;
  int y;
};

struct tri
{
  struct coord v[3];
};

union iap
{
  int i;
  void *p;
};

void test_1 (void)
{
  struct coord c = {3, 4};
  __analyzer_eval (c.x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c.y == 4); /* { dg-warning "TRUE" } */  
}

void test_2 (void)
{
  struct coord c = {3};
  __analyzer_eval (c.x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c.y == 0); /* { dg-warning "TRUE" } */  
}

void test_3 (void)
{
  struct coord c = {};
  __analyzer_eval (c.x == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (c.y == 0); /* { dg-warning "TRUE" } */  
}

void test_4 (void)
{
  int c[2] = {3, 4};
  __analyzer_eval (c[0] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1] == 4); /* { dg-warning "TRUE" } */  
}

void test_5 (void)
{
  int c[2] = {3};
  __analyzer_eval (c[0] == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1] == 0); /* { dg-warning "TRUE" } */  
}

void test_6 (void)
{
  int c[2] = {};
  __analyzer_eval (c[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1] == 0); /* { dg-warning "TRUE" } */  
}

void test_7 (void)
{
  struct coord c[2] = {{3, 4}, {5, 6}};
  __analyzer_eval (c[0].x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[0].y == 4); /* { dg-warning "TRUE" } */  
  __analyzer_eval (c[1].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1].y == 6); /* { dg-warning "TRUE" } */  
}

void test_8 (void)
{
  struct coord c[2] = {{3}, {5}};
  __analyzer_eval (c[0].x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[0].y == 0); /* { dg-warning "TRUE" } */  
  __analyzer_eval (c[1].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1].y == 0); /* { dg-warning "TRUE" } */  
}

void test_9 (void)
{
  struct coord c[2] = {{}, {}};
  __analyzer_eval (c[0].x == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[0].y == 0); /* { dg-warning "TRUE" } */  
  __analyzer_eval (c[1].x == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1].y == 0); /* { dg-warning "TRUE" } */  
}

void test_10 (void)
{
  struct coord c[2] = {{.x = 3, .y = 4}, {5, 6}};
  __analyzer_eval (c[0].x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[0].y == 4); /* { dg-warning "TRUE" } */  
  __analyzer_eval (c[1].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1].y == 6); /* { dg-warning "TRUE" } */  
}

void test_11 (void)
{
  struct coord c[2] = {{.y = 4}, {5, 6}};
  __analyzer_eval (c[0].x == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[0].y == 4); /* { dg-warning "TRUE" } */  
  __analyzer_eval (c[1].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (c[1].y == 6); /* { dg-warning "TRUE" } */  
}

void test_12 (void)
{
  struct tri t = {};
  __analyzer_eval (t.v[0].x == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (t.v[2].y == 0); /* { dg-warning "TRUE" } */  
}

void test_13 (void)
{
  struct tri t = {3, 4, 5, 6, 7, 8};
  __analyzer_eval (t.v[0].x == 3); /* { dg-warning "TRUE" } */
  __analyzer_eval (t.v[0].y == 4); /* { dg-warning "TRUE" } */
  __analyzer_eval (t.v[1].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (t.v[1].y == 6); /* { dg-warning "TRUE" } */
  __analyzer_eval (t.v[2].x == 7); /* { dg-warning "TRUE" } */
  __analyzer_eval (t.v[2].y == 8); /* { dg-warning "TRUE" } */
}

void test_14 (void)
{
  union iap u = {};
  __analyzer_eval (u.i == 0); /* { dg-warning "TRUE" } */
}
