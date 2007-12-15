/* Make sure we don't emit strings just because of debug information
   for string initializers.  */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */
/* { dg-final { scan-assembler-not "dontgenerate" } } */
static const char *p = "dontgenerate1";
static const char *q[2] = { 0, "dontgenerate2" };
