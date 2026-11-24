/* Ensure that per-lane loads from LDS use the masked load pattern for
   32-bit DS addresses in SIMT mode.

   In SIMT mode each load is masked by EXEC, and the @mov<mode>_exec
   expander turns it into an UNSPEC_MASKLOAD insn, so that the MEM cannot
   later be separated from the mask and loaded unmasked.  LDS addresses are
   32-bit (V64SI), unlike the 64-bit flat/global addresses that
   load<mode>_exec takes, so they need load<mode>_exec_ds.  Without it the
   LDS address was forced into a 64-bit vector register and loaded with
   flat_load.  */

/* { dg-do compile } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-options "-O1 -dp" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

__lds int ai[16];
__lds long al[16];

int
load_si (int i)
{
  return ai[i];
}

long
load_di (int i)
{
  return al[i + 3];
}

int
load_cond (int c, int i)
{
  int r = 0;
  if (c)
    r = ai[i];
  return r;
}

/* { dg-final { scan-assembler-times {ds_read_b32\tv[0-9]+, v[0-9]+ offset:[0-9]+[^\n]*loadv64si_exec_ds} 2 } } */
/* { dg-final { scan-assembler-times {ds_read_b64\tv\[[0-9]+:[0-9]+\], v[0-9]+ offset:[0-9]+[^\n]*loadv64di_exec_ds} 1 } } */
/* { dg-final { scan-assembler-not "flat_load" } } */
