/* PR optimization/12198

   This was a miscompilation of a switch expressions because
   the "Case Ranges" extension wasn't handled in tree-cfg.c.  */

/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

extern void abort (void);
extern void exit (int);

int main() 
{ 
   int i; 
   i = 2; 
   switch (i) 
      { 
      case 1 ... 5: 
         goto L1; 
      default: 
         abort (); 
         goto L1; 
      } 
   L1: 
   exit(0); 
}

/* The abort() call clearly is unreachable.  */
/* { dg-final { scan-tree-dump-times "abort" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
