/* { dg-do compile } */
/* { dg-options "-O2 -fno-inline -march=loongarch64 -mabi=lp64d -O2 -mcmodel=extreme -fno-plt -mexplicit-relocs=always -mdirect-extern-access" } */

struct A {
  virtual ~A();
};

struct B : virtual A {};
void var() { B(); }

/* { dg-final { scan-assembler "pcalau12i\t\[^\n\]*%pc_hi20\\(\\.LTHUNK0\\)\n\taddi\\.d\t\[^\n\]*%pc_lo12\\(\\\.LTHUNK0\\)\n\tlu32i\\.d\t\[^\n\]*%pc64_lo20\\(\\.LTHUNK0\\)\n\tlu52i\\.d\t\[^\n\]*%pc64_hi12\\(\\.LTHUNK0\\)" } } */
