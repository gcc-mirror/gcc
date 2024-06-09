/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned uint32_t;
int cde40_t;
int offset;
void aal_test_bit();
uint32_t cde40_key_pol();
long cde40_offset_check(uint32_t pos) {
  cde40_key_pol();
  if (cde40_t)
    return (offset - 2) % (((pos == 3) ? 18 : 26)) != 0;
  return 0;
}
void cde40_check_struct() {
  uint32_t i, j, to_compare;
  for (;; i++) {
    cde40_offset_check(i);
    if (to_compare == 0) {
      if (i && cde40_key_pol())
	;
      to_compare = i;
      continue;
    }
    j = to_compare;
    for (; j < i; j++)
      aal_test_bit();
  }
}
