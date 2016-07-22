/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-code-hoisting -fdump-tree-pre-stats" } */
double cos (double);
double f(double a)
{
  double b;
  double c,d;
 if (a < 2.0)
   {
     c = cos (a);
   }
 else
   {
     c = 1.0; 
   }
 d = cos (a);
 return d + c;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
