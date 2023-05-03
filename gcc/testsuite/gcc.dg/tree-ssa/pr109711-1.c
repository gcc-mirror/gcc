// { dg-do compile }
// { dg-options "-O2" }

void lspf2lpc();

void interpolate_lpc(int subframe_num) {
  float weight = 0.25 * subframe_num + 1;
  if (weight)
    lspf2lpc();
}

void qcelp_decode_frame() {
  int i;
  for (;; i++)
    interpolate_lpc(i);
}
