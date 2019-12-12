/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  Test #pragma pack diagnostics.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */
/* { dg-require-ascii-locale "" } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

#pragma pack(push)
#pragma pack(pop, ó) /* { dg-warning "pop, \\\\U000000f3.*push, \\\\U000000f3" } */
#pragma pack(ç) /* { dg-warning "unknown action '\\\\U000000e7'" } */

