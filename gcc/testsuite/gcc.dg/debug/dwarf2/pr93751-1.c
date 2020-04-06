// { dg-do compile }
// { dg-options "-O -gdwarf-2 -g1 -dA" }
static int bar;

// Verify that with -g1 we still do not generate DIEs for static variables.
// { dg-final { scan-assembler-not " DW_TAG_variable" } }
