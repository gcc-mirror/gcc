/* { dg-options "-mthumb -Os" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "ands" } } */

struct S {
      int bi_buf;
      int bi_valid;
};

int tz (struct S* p, int bits, int value)
{
     if (p == 0) return 1;
      p->bi_valid = bits;
      p->bi_buf = value & ((1 << bits) - 1);
      return 0;
}
