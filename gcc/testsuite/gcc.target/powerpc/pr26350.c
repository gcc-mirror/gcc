/* { dg-do compile { target { powerpc*-*-darwin* powerpc*-*-aix* rs6000-*-* powerpc*-*-linux* } } } */
/* { dg-options "-O2 -mlong-double-128 -fpic" } */
/* { dg-require-effective-target fpic } */

typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef struct REGS REGS;
typedef union { uint32_t F; } FW;
typedef union { struct { FW L; } F; } DW;
typedef struct _PSW {
  DW ia;
} PSW;
struct REGS {
  PSW psw;
  DW cr[16];
};
struct ebfp {
  long double v;
};
void put_ebfp (struct ebfp *);

void s390_convert_fix32_to_bfp_ext_reg (REGS *regs)
{
  struct ebfp op1;
  int32_t op2;
  ((regs))->psw.ia.F.L.F += (4);
  if(!((regs)->cr[(0)].F.L.F & 0x00040000))
    op1.v = (long double)op2;
  put_ebfp(&op1);
}
