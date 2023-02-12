/* Test BTF generation for extern const void symbols.
   BTF_KIND_VAR records should be emitted for such symbols if they are used,
   as well as a corresponding entry in the appropriate DATASEC record.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Expect 1 variable record only for foo, with 'extern' (2) linkage.  */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*btv_info" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*btv_linkage" 1 } } */

/* { dg-final { scan-assembler-times "ascii \"foo.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */

/* { dg-final { scan-assembler-times "0\[\t \]+\[^\n\]*bts_offset" 1 } } */
/* { dg-final { scan-assembler-times "1\[\t \]+\[^\n\]*bts_size" 1 } } */

extern const void foo __attribute__((weak)) __attribute__((section (".ksyms")));
extern const void bar __attribute__((weak)) __attribute__((section (".ksyms")));

unsigned long func () {
  unsigned long x = (unsigned long) &foo;

  return x;
}

