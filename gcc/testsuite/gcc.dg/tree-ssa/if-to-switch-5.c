/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized" } */

int crud (unsigned char c)
{
  return (((((((((((int) c == 46) || (int) c == 44)
		 || (int) c == 58) || (int) c == 60) || (int) c == 62)
	      || (int) c == 64) || (int) c == 34) || (int) c == 92)
	   || (int) c == 39) != 0);
}

/* { dg-final { scan-tree-dump "Condition chain with \[^\n\r]\* BBs transformed into a switch statement." "iftoswitch" } } */
