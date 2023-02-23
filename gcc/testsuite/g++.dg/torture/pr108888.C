// { dg-do compile }

int scaleValueSaturate_scalefactor, scaleValueSaturate___trans_tmp_2,
    scaleValuesSaturate_i;
int scaleValueSaturate(int value) {
  int result = __builtin_clz(value);
  if (value)
    if (-result <= scaleValueSaturate_scalefactor)
      return 0;
  return scaleValueSaturate___trans_tmp_2;
}
short scaleValuesSaturate_dst;
short *scaleValuesSaturate_src;
void scaleValuesSaturate() {
  for (; scaleValuesSaturate_i; scaleValuesSaturate_i++)
    scaleValuesSaturate_dst =
        scaleValueSaturate(scaleValuesSaturate_src[scaleValuesSaturate_i]);
}
