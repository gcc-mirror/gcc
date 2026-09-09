/* { dg-do compile } */
/* { dg-options "-O -Wmaybe-uninitialized" } */

_Bool g18, f1_c17;
void *f1_a0;
void f1() {
  unsigned char init8;
  _Bool c15, c18 = 0;
lbl_f2_b0:
  if (g18)
  lbl_f2_b1:
  lbl_f2_b4:
  lbl_f2_b8:
    if (f1_c17)
    lbl_f2_b9:
      if (f1_c17)
        goto lbl_f2_b28;
lbl_f2_b14:
  if (g18)
    goto lbl_f2_b62;
lbl_f2_b28:
  init8 = c18;
  switch (init8) {
  case 9:
    goto lbl_f2_b1;
  case 13:
    goto lbl_f2_b9;
  case 1:
    goto lbl_f2_b0;
  case 11:
  case 25:
    goto lbl_f2_b4;
  }
lbl_f2_b62:
  c18 = 80;
  if (c15) /* { dg-warning "may be used uninitialized" } */
    goto lbl_f2_b1;
  c15 = f1_a0;
  goto lbl_f2_b14;
}
