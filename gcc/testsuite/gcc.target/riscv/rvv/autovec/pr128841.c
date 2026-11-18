/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcbv -mabi=lp64d -mrvv-max-lmul=conv-dynamic" } */

short selectend_nurb_bp_1;
char selectend_nurb_sel;
void swap_selection_bpoint();
void selectend_nurb(char doswap) {
  int nu_3;
  while (nu_3--)
    if (selectend_nurb_bp_1) {
      if (doswap)
        swap_selection_bpoint();
      else
        selectend_nurb_sel = 1;
      if (selectend_nurb_sel)
        break;
    }
}
