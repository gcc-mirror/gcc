/* PR tree-optimization/101496 */
/* { dg-do compile } */
/* { dg-options "-O2 " } */

int c_1, li_2, us_3, func_14_s_5;

void func_14() {
  {
    unsigned uli_8 = 0;
  lbl1806324B:
    if (uli_8 /= us_3 |= func_14_s_5 < 0 | func_14_s_5 != c_1) {
      uli_8 += c_1 >= us_3;
      if (uli_8)
        ;
      else
        li_2 &&func_14_s_5 <= c_1 ?: 0;
      unsigned *ptr_9 = &uli_8;
    }
  }
  goto lbl1806324B;
}

