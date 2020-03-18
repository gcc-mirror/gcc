// { dg-do compile }
// { dg-options "-O -gdwarf-2 -g1 -dA" }
int foo;

// Verify that with -g1 we generate DIEs for external variables.
// { dg-final { scan-assembler " DW_TAG_variable" } }
