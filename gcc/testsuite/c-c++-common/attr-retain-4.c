/* { dg-do compile { target gnu_retain } } */
/* { dg-options "-Wall -O2 -fcommon" } */
/* Alpha places small objects in gp-relative sdata/sbss, which carry an
   extra "s" section flag the scans do not match. */
/* { dg-additional-options "-G0" { target alpha*-*-* } } */

int xyzzy __attribute__((__used__, __retain__)); 

/* { dg-final { scan-assembler "xyzzy" } } */
/* { dg-final { scan-assembler ",\"awR\"" { target gnu_retain } } } */
