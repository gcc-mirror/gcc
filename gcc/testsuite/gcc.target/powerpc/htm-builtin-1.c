/* { dg-do assemble { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_htm_ok } */
/* { dg-options "-O2 -mhtm -save-temps" } */

/* { dg-final { scan-assembler-times "tbegin\\." 1 } } */
/* { dg-final { scan-assembler-times "tend\\." 2 } } */
/* { dg-final { scan-assembler-times "tabort\\." 2 } } */
/* { dg-final { scan-assembler-times "tabortdc\\." 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "tabortdci\\." 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "tabortwc\\." 1 } } */
/* { dg-final { scan-assembler-times "tabortwci\\." 2 } } */
/* { dg-final { scan-assembler-times "tcheck" 1 } } */
/* { dg-final { scan-assembler-times "trechkpt\\." 1 } } */
/* { dg-final { scan-assembler-times "treclaim\\." 1 } } */
/* { dg-final { scan-assembler-times "tsr\\." 3 } } */
/* { dg-final { scan-assembler-times "mfspr" 4 } } */
/* { dg-final { scan-assembler-times "mtspr" 4 } } */

void use_builtins (long *p, char code, long *a, long *b)
{
  p[0] = __builtin_tbegin (0);
  p[1] = __builtin_tend (0);
  p[2] = __builtin_tendall ();
  p[3] = __builtin_tabort (0);
  p[4] = __builtin_tabort (code);

#ifdef __powerpc64__
  p[5] = __builtin_tabortdc (0xf, a[5], b[5]);
  p[6] = __builtin_tabortdci (0xf, a[6], 13);
#endif
  p[7] = __builtin_tabortwc (0xf, a[7], b[7]);
  p[8] = __builtin_tabortwci (0xf, a[8], 13);

  p[9] = __builtin_tcheck ();
  p[10] = __builtin_trechkpt ();
  p[11] = __builtin_treclaim (0);
  p[12] = __builtin_tresume ();
  p[13] = __builtin_tsuspend ();
  p[14] = __builtin_tsr (0);
  p[15] = __builtin_ttest (); /* This expands to a tabortwci.  */


  p[16] = __builtin_get_texasr ();
  p[17] = __builtin_get_texasru ();
  p[18] = __builtin_get_tfhar ();
  p[19] = __builtin_get_tfiar ();

  __builtin_set_texasr (a[20]);
  __builtin_set_texasru (a[21]);
  __builtin_set_tfhar (a[22]);
  __builtin_set_tfiar (a[23]);
}
