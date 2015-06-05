/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -ftree-vectorize" }  */
/* { dg-add-options arm_neon } */


void
_op_blend_p_caa_dp(unsigned *s, unsigned* e, unsigned *d, unsigned c) {
  while (d < e) {
    *d = ( (((((*s) >> 8) & 0x00ff00ff) * (c)) & 0xff00ff00) + (((((*s) & 0x00ff00ff) * (c)) >> 8) & 0x00ff00ff) );
    d++;
    s++;
  }
}

/* These constants should be emitted as immediates rather than loaded from memory.  */

/* { dg-final { scan-assembler-not "(\\.d?word)" } } */
