/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d" } */
/* { dg-final { scan-assembler-not "bstrpick" } } */

/* Test for loongarch_extend_comparands patch.  */
extern void asdf (int);
void
foo (signed char x) {
    switch (x) {
      case 0: asdf (10); break;
      case 1: asdf (11); break;
      case 2: asdf (12); break;
      case 3: asdf (13); break;
      case 4: asdf (14); break;
    }
}
