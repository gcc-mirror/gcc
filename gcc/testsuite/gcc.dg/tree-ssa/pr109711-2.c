// { dg-do compile }
// { dg-options "-O2" }

void lspf2lpc();

int interpolate_lpc_q_0;

void
interpolate_lpc(int subframe_num) {
  float weight;
  if (interpolate_lpc_q_0)
    weight = subframe_num;
  else
    weight = 1.0;
  if (weight != 1.0)
    lspf2lpc();
}

void
qcelp_decode_frame() {
  int i;
  for (;; i++)
    interpolate_lpc(i);
}
