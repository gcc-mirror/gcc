/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-details" } */

 int i; 
 int foo(int q) { 
     int j;
     int p;
     for (j = 0; j < 9; j++)
       {
	 p = i + q;
       }
     return p;
 }
/* We should replace p = a load from i that will pop into the loop, with a hoisted version.
   We should also replace i + q with a hoisted version.  */
/* { dg-final { scan-tree-dump-times "Replaced " 2 "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
