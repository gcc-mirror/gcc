/* { dg-do compile } */
/* { dg-options "-O2" } */

int i_0, c_4, uc_7, func_2_c_11;

short *func_2_ptr_10;

void func_2() {
  uc_7 = 7;
  for (; uc_7 <= 60; uc_7 += 1) {
    c_4 = 5;
    for (; c_4 <= 76; c_4 += 1) {
      func_2_ptr_10 = &i_0;	/* { dg-warning "assignment to .*" } */
      if ((i_0 |= 5) > 0 ?: (60 && uc_7) | *func_2_ptr_10)
        if (func_2_c_11)
          for (;;)
            ;
    }
  }
}
