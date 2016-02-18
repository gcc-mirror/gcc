/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-mcpu=power7 -mtune=power8 -O3 -mcmodel=medium" } */

#define SIZE 4
struct foo {
  unsigned char uc;
  signed char sc;
  unsigned short us;
  short ss;
  int i;
  unsigned u;
};

static struct foo st[SIZE];
struct foo *ptr_st = &st[0];

int fusion_uchar (void){ return st[SIZE-1].uc; }
int fusion_schar (void){ return st[SIZE-1].sc; }
int fusion_ushort (void){ return st[SIZE-1].us; }
int fusion_short (void){ return st[SIZE-1].ss; }
int fusion_int (void){ return st[SIZE-1].i; }
unsigned fusion_uns (void){ return st[SIZE-1].u; }

/* { dg-final { scan-assembler-times "gpr load fusion"    6 } } */
/* { dg-final { scan-assembler-times "lbz"                2 } } */
/* { dg-final { scan-assembler-times "extsb"              1 } } */
/* { dg-final { scan-assembler-times "lhz"                2 } } */
/* { dg-final { scan-assembler-times "extsh"              1 } } */
/* { dg-final { scan-assembler-times "lwz"                2 } } */
