/* { dg-do compile } */
/* { dg-options "-O0 -g -dA" } */

// Test that there is only one DW_TAG_unspecified_parameters DIE.

void
foo (const char *format, ...)
{
}

// { dg-final { scan-assembler-times "DIE.*DW_TAG_unspecified_parameters" 1 { xfail { powerpc-ibm-aix* } } } }
