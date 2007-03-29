/* { dg-do compile { target *-*-darwin9* } } */
/* { dg-final { scan-assembler ".comm _foo,1,15" } } */

char foo __attribute__ ((aligned(32768)));
