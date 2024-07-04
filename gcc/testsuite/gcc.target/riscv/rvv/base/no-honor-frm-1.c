/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

void foo (void) {
  for (unsigned i = 0; i < sizeof(foo); i++)
    __builtin_printf("%d", i);
}

/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[01234]} } } */
/* { dg-final { scan-assembler-not {fsrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {frrm\s+[axs][0-9]+} } } */
