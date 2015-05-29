/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" }  */

/* We may not optimize this to b->t[i] = 1.  */

struct a
{
  char t[10];
};

struct a *b;

void f(__SIZE_TYPE__ i)
{
  char *c = b->t;
  c[i] = 1;
}

/* { dg-final { scan-tree-dump-times "\\\[\[^\n\r\]*\\\] = 1;" 0 "forwprop1" } } */
