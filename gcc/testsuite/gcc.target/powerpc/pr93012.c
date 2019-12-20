/* PR target/93012 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -std=c99" } */

unsigned long long msk66() { return 0x6666666666666666ULL; }
unsigned long long mskih() { return 0xabcd1234abcd1234ULL; }
unsigned long long mskh0() { return 0x0000123400001234ULL; }
unsigned long long mskl0() { return 0xabcd0000abcd0000ULL; }
unsigned long long mskh1() { return 0xffff9234ffff9234ULL; }
unsigned long long mskl1() { return 0x2bcdffff2bcdffffULL; }
unsigned long long mskse() { return 0xffff1234ffff1234ULL; }

/* { dg-final { scan-assembler-times {\mrldimi\M} 7 } } */
