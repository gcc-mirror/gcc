/* This testcase ICEd on alpha because of an unrecognized insn formed
   by conditional move optimization using an incorrect mode.  */
/* { dg-do compile } */
/* { dg-options "-O -ffast-math" } */

char*
barf (double x)
{
    return (x<0.0) ? "foo" : "bar";
}
