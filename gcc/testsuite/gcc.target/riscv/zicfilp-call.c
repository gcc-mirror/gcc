/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-O2 -fPIE -march=rv64gc_zicfilp -mabi=lp64d -fcf-protection=branch" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

extern void _dl_find_object_init (void);

void
_dl_non_dynamic_init (void)
{
  extern __typeof__ (_dl_find_object_init) _dl_find_object_init __attribute__ ((weak));
  (_dl_find_object_init != ((void *) 0) ? _dl_find_object_init () : (void)0);
}

/* { dg-final { scan-assembler-times "mv\tt2" 1 } } */
