/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

long tar_atol256_max, tar_atol256_size, tar_atosl_min;
char tar_atol256_s;
void __errno_location();


inline static long tar_atol256(long min) {
  char c;
  int sign;
  c = tar_atol256_s;
  sign = c;
  while (tar_atol256_size) {
    if (c != sign)
      return sign ? min : tar_atol256_max;
    c = tar_atol256_size--;
  }
  if ((c & 128) != (sign & 128))
    return sign ? min : tar_atol256_max;
  return 0;
}

inline static long tar_atol(long min) {
  return tar_atol256(min);
}

long tar_atosl() {
  long n = tar_atol(-1);
  if (tar_atosl_min) {
    __errno_location();
    return 0;
  }
  if (n > 0)
    return 0;
  return n;
}
