/* { dg-do compile { target *-*-darwin[912]* } } */
/* { dg-options "-fcommon" } */
/* { dg-final { scan-assembler ".comm\[ \t\]_foo,1,15" } } */

char foo __attribute__ ((aligned(32768)));
