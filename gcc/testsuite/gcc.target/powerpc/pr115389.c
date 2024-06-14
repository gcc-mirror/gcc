/* PR target/115389 */
/* { dg-do assemble } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect -mno-vsx -mno-altivec -mabi=no-altivec -save-temps" } */
/* { dg-require-effective-target rop_ok } */

/* Verify we do not emit invalid offsets for our ROP insns.  */

extern void foo (void);
long
bar (void)
{
  foo ();
  return 0;
}

/* { dg-final { scan-assembler-times {\mhashst\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhashchk\M} 1 } } */
