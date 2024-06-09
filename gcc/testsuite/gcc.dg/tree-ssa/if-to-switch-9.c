/* PR tree-optimization/88702 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch-optimized" } */
/* { dg-additional-options "--param=case-values-threshold=3" { target { avr-*-* } } } */

int IsHTMLWhitespace(int aChar) {                         
  return aChar == 0x0009 || aChar == 0x000A ||              
         aChar == 0x000C || aChar == 0x000D ||              
         aChar == 0x0020;                                             
}

/* { dg-final { scan-tree-dump "Condition chain with \[^\n\r]\* BBs transformed into a switch statement." "iftoswitch" } } */
