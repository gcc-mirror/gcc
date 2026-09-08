unsigned short g6, g20;
bool g24_a1;
void g24() {
  bool c10 = 1, c14, c15;
  unsigned v13;
lbl_f11_b25:
  if (g24_a1)
    goto lbl_f11_b29;
lbl_f11_b27:
  goto lbl_f11_b32;
lbl_f11_b29:
  g24_a1 = 0;
  short __ov_tmp_g20;
  c14 = __builtin_mul_overflow(g6, g20, &__ov_tmp_g20);
  if (c10)
    goto lbl_f11_b32;
lbl_f11_b32:
  v13 = c15 ? ~00 : 0;
  c15 = g24_a1 ^ c14 ^ 1 ^ 1;
  (void)__builtin_clz(v13);
  if (c14)
    goto lbl_f11_b27;
  c10 = 0;
  goto lbl_f11_b25;
}

