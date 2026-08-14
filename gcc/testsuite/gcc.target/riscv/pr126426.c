/* { dg-do run } */
/* { dg-additional-options "--std=gnu99 -march=rv64gcb" { target rv64 } } */
/* { dg-additional-options "--std=gnu99 -march=rv32gcb" { target rv32 } } */

typedef union {
  short mv[2];
  unsigned mv32;
} MotionVector;
enum { kProjectionMvClamp, kMaxFrameDistance };
short kProjectionMvDivisionLookup[] = {0, 6};
int SpecGetMvProjectionKernel(int mv, int numerator, int denominator) {
  int value = mv * numerator * kProjectionMvDivisionLookup[denominator];
  if (value >= 0)
    value += 3;
  value >>= 4;
  if (value > 4)
    value = 1;
  return value;
}
void SpecGetMvProjectionNoClamp(MotionVector mv, int numerator, int denominator,
                                MotionVector *projection_mv) {
  for (int i = 0; i < 2; ++i)
    projection_mv->mv[i] =
        SpecGetMvProjectionKernel(mv.mv[i], numerator, denominator);
}
int main() {
  short mvs[5][2] = {{0}};
  for (int j = 0; j < 5; ++j) {
    short *mv_value = mvs[j];
    for (int numerator = -kMaxFrameDistance; numerator; ++numerator)
      for (int denominator = 0; denominator <= kMaxFrameDistance;
           ++denominator) {
        MotionVector mv, spec_projection_mv;
        mv.mv[0] = mv.mv[1] = mv_value[1];
        SpecGetMvProjectionNoClamp(mv, numerator, denominator,
                                   &spec_projection_mv);
        if (spec_projection_mv.mv32) {
           __builtin_abort ();
	}
      }
  }
  __builtin_exit (0);
}
