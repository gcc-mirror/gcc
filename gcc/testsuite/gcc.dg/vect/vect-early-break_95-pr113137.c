/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-w" } */

short gen_to_words_words;
void gen_to_words() {
  unsigned short *lp = &gen_to_words_words;
  long carry;
  for (carry = 1, lp--; carry; lp--) {
    carry = *lp + carry;
    *lp = carry >>= 16;
    if (lp == &gen_to_words_words)
      break;
  }
}
