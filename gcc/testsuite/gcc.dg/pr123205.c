/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
void isconst (int, int);
void nonconst (int, int);

int foo (int x)
{
 int y =  __builtin_constant_p (x);
 if (y)
   isconst (y, x);
  else
   nonconst (y, x);

 if (x == 24)
   {
     /* Y should have the same value as earlier. */
     if (y)
       isconst (y, x);
     else
       nonconst (y, x);
   }
}
/* { dg-final { scan-tree-dump-not "isconst"  "optimized" } } */
