/* { dg-do compile { target *-*-darwin[912]* } } */
/* { dg-final { scan-assembler ".comm _foo,1,15" } } */

char foo __attribute__ ((aligned(32768)));
