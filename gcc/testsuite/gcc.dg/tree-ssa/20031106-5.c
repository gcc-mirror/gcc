/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized" } */

extern void link_error (void);

/* Check for cprop on different fields of same type structs.  */

struct s
{
  char d;
  int a, b;
  double m;
};

void foo2 (struct s*  r, struct s*  p)
{
  r->a = 0;
  p->b = 1;
  r->a++;
  p->b++;
  if (r->a != 1)
    link_error ();
  if (p->b != 2)
    link_error ();
}

/* There should be no link_error calls.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" { xfail *-*-* } } } */
