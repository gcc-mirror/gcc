/* PR debug/37726 */
/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } } */

int foo (int parm)
{
  int var = 0;
  int bar (void)
  {
    return parm + var; /* BREAK */
  }
  parm++;              /* BREAK */
  var++;
  return bar ();
}

int
main (void)
{
  return foo (4) - 6;
}

/* { dg-final { gdb-test 11 "parm" "5" } } */
/* { dg-final { gdb-test 11 "var"  "1" } } */
/* { dg-final { gdb-test 13 "parm" "4" } } */
/* { dg-final { gdb-test 13 "var"  "0" } } */
