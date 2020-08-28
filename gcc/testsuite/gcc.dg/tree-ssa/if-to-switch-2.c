/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized" } */

int IsHTMLWhitespaceNoRange(int aChar)
{
  return aChar == 0x0001 || aChar == 0x000A ||
         aChar == 0x000C || aChar == 0x000E ||
         aChar == 0x0020;
}

/* { dg-final { scan-tree-dump "Condition chain with 3 BBs transformed into a switch statement." "iftoswitch" } } */
