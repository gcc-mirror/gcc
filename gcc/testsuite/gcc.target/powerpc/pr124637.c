/* { dg-do compile { target { powerpc64-*-* } } } */
/* { dg-options "-O2 -m64 -mbig-endian" } */

typedef struct {
    unsigned short a;
    unsigned short b;
    unsigned short c;
} S_t;

static const S_t m1 = { 0x20, 1, 1 };

void ext(S_t r);
void foo() { ext(m1); }

/* Apologies in advance that these may be a little fragile.  */

/* { dg-final { scan-assembler "lis 3,0x20" } } */
/* { dg-final { scan-assembler "ori 3,3,0x1" } } */
/* { dg-final { scan-assembler "sldi 3,3,16" } } */
/* { dg-final { scan-assembler "ori 3,3,0x1" } } */

/* { dg-final { scan-assembler-not "lis 9,0x20" } } */
/* { dg-final { scan-assembler-not "ori 9,9,0x1" } } */
/* { dg-final { scan-assembler-not "lis 3,0x1" } } */
/* { dg-final { scan-assembler-not "rldimi 3,9,32,0" } } */

