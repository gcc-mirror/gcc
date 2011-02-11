/* Verify diagnostics for extended identifiers refer to UCNs (in the C
   locale).  Test #pragma pack diagnostics.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -fextended-identifiers" } */
/* { dg-require-ascii-locale "" } */

#pragma pack(push)
#pragma pack(pop, \u00f3) /* { dg-warning "pop, \\\\U000000f3.*push, \\\\U000000f3" } */
#pragma pack(\u00e7) /* { dg-warning "unknown action '\\\\U000000e7'" } */
