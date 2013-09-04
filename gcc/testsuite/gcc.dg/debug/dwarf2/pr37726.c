/* PR debug/37726 */
/* { dg-do compile } */
/* { dg-options "-gdwarf -O0 -dA -fno-merge-debug-strings" } */

int foo (int parm)
{
  int var = 0;
  int bar (void)
  {
    return parm + var;
  }
  parm++;
  var++;
  return bar ();
}

int
main (void)
{
  return foo (4) - 6;
}

/* Both parm and var variables should be in debug info for both foo and bar.  */
/* { dg-final { scan-assembler-times "\"parm\[^\n\]*\"\[^\n\]*DW_AT_name" 2 } } */
/* { dg-final { scan-assembler-times "\"var\[^\n\]*\"\[^\n\]*DW_AT_name" 2 } } */
