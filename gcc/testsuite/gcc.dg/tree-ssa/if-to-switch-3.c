/* { dg-do compile } */
/* Supplying case-values-threshold makes the testcase less target dependent.  */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized --param case-values-threshold=4" } */

int IsMySuperRandomChar(int aChar)
{
  return aChar == 0x0009 || aChar == 0x000A ||
         aChar == 0x000C || aChar == 0x000D ||
         aChar == 0x0020 || aChar == 0x0030;
}

/* { dg-final { scan-tree-dump "Condition chain with \[^\n\r]\* BBs transformed into a switch statement." "iftoswitch" } } */
