/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-require-linker-plugin "" } */
/* { dg-options "-fpic -shared -O2 --save-temps -mexplicit-relocs=auto -flto -fuse-linker-plugin -flto-partition=one" } */

int pcrel __attribute__ ((visibility ("hidden")));
int got __attribute__ ((visibility ("default")));

int
*addr_pcrel (void)
{
  return &pcrel;
}

int
*addr_got (void)
{
  return &got;
}

/* With linker plugin we should use la.local (it can be relaxed to pcaddi),
   but not la.global (we are pretty sure the linker cannot relax la.global
   got).  */
/* { dg-final { scan-lto-assembler "la.local.*pcrel" } } */
/* { dg-final { scan-lto-assembler "pcalau12i.*%got_pc_hi20\\\(got\\\)" } } */
/* { dg-final { scan-lto-assembler "ld.*%got_pc_lo12\\\(got\\\)" } } */
