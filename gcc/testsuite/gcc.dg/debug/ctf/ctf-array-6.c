/* CTF generation for multidimensional array.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gctf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]+0x2\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]+0x3\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]+0x4\[\t \]+\[^\n\]*cta_nelems" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]+0x1\[\t \]+\[^\n\]*cta_contents\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*0x4\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]+0x3\[\t \]+\[^\n\]*cta_contents\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*0x3\[\t \]+\[^\n\]*cta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]+0x4\[\t \]+\[^\n\]*cta_contents\[\\r\\n\]+\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*0x2\[\t \]+\[^\n\]*cta_nelems" 1 } } */

int a[2][3][4];
