/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O3" } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-final { scan-assembler "lxvx" } } */
/* { dg-final { scan-assembler "stxvx" } } */
/* { dg-final { scan-assembler-not "lxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

/* Verify P9 vector loads and stores are used rather than the
   load-swap/swap-store workarounds for P8.  */
#define SIZE (16384/sizeof(__float128))

static __float128 x[SIZE] __attribute__ ((aligned (16)));
static __float128 y[SIZE] __attribute__ ((aligned (16)));
static __float128 a;

void obfuscate(void *a, ...);

void __attribute__((noinline)) do_one(void)
{
  unsigned long i;

  obfuscate(x, y, &a);

  for (i = 0; i < SIZE; i++)
    y[i] = a * x[i];

  obfuscate(x, y, &a);
}
