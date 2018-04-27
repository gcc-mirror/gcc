/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-rvrp" } */
extern void abort(void);

void
foo (int x)
{
 int y;
 if (x > -4 && x < 0)
   {
     y = x * x;
     if (y < 1 || y > 9)
       abort();
   }
}

/* { dg-final { scan-tree-dump "Branch rewritten" "rvrp"} } */
