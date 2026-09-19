/* PR target/127374 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcb_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gcb_zicond -mabi=ilp32d" { target { rv32 } } } */

int sel_le_s (int x, int y) { return x <= 100 ? y : 0; }
int sel_gt_s (int x, int y) { return x >  100 ? y : 0; }
int sel_le_u (unsigned x, int y) { return x <= 100 ? y : 0; }
int sel_gt_u (unsigned x, int y) { return x >  100 ? y : 0; }

int clr_le_s (int x, int y) { return x <= 100 ? 0 : y; }
int clr_gt_s (int x, int y) { return x >  100 ? 0 : y; }
int clr_le_u (unsigned x, int y) { return x <= 100 ? 0 : y; }
int clr_gt_u (unsigned x, int y) { return x >  100 ? 0 : y; }

/* { dg-final { scan-assembler-times {\mslti\s+\w+,\w+,101\M} 4 } } */
/* { dg-final { scan-assembler-times {\msltiu\s+\w+,\w+,101\M} 4 } } */
/* { dg-final { scan-assembler-times {\mczero\.eqz\M} 4 } } */
/* { dg-final { scan-assembler-times {\mczero\.nez\M} 4 } } */
/* { dg-final { scan-assembler-not {\mli\M} } } */
