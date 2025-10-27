/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcbv_zicond -mabi=lp64d" { target rv64 } } */
/* { dg-options "-O2 -march=rv32gcbv_zicond -mabi=ilp32" { target rv32 } } */



unsigned int test0 (unsigned int x) { return ((x >> 4) ^ 1) & 1; }

unsigned int test1 (unsigned int x) { return ((x >> 4) & 1) ^ 1; }

unsigned int test2 (unsigned int x) { return ~(x >> 4) & 1; }

unsigned int test3 (unsigned int x) { return ((~x >> 4) & 1); }

unsigned int test4 (unsigned int x) { return (x >> 4) & 1; }

int test5 (int vi) { return vi - (((vi >> 6) & 0x01) << 1); }

int test6 (int vi) { return vi - (((vi >> 6) & 0x01) << 1) - 1; }


/* { dg-final { scan-assembler-times "\\tbexti" 5 } } */
/* { dg-final { scan-assembler-times "\\txori" 3 } } */
/* { dg-final { scan-assembler-times "\\tnot" 1 } } */
/* { dg-final { scan-assembler-times "\\tsrli" 2 } } */
/* { dg-final { scan-assembler-times "\\tandi" 2 } } */
/* { dg-final { scan-assembler-times "\\tsub" 2 } } */
/* { dg-final { scan-assembler-times "\\taddi" 1 } } */


