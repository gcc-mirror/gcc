/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ext_dce" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void
foo(short *d, short *tmp) {
    int x = d[0] + d[1];
    int y = d[2] + d[3];
    tmp[0] = x + y;
    tmp[1] = x - y;
}

/* { dg-final { scan-assembler-not {\mzext\.h\M} } } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */
