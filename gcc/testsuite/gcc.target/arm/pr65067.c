/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m3_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_cpu_cortex_m3 } */

struct tmp {
 unsigned int dummy;
 union {
  struct {
   unsigned int xyz : 1;
   unsigned int mode: 3;
   unsigned int res : 28;
  } bf;
  unsigned int wordval;
 } reg;
};

void set_mode(int mode)
{
 volatile struct tmp *t = (struct tmp *) 0x1000;
 t->reg.bf.mode = mode;
}

/* { dg-final { scan-assembler "bfi" } } */
