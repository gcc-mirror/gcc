/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */


struct sv XS_constant__make_const_svz_2_0;
struct sv {
  int sv_flags;
} XS_constant__make_const() {
  struct sv *sv = &XS_constant__make_const_svz_2_0;
  sv->sv_flags |= 65536;
  if (XS_constant__make_const_svz_2_0.sv_flags & 5)
    for (;;)
      ;
}

/* { dg-final { scan-assembler-not "ori\t" } } */

