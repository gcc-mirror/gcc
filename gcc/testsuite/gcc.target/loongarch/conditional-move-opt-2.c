/* { dg-do compile } */
/* { dg-options "-O2 --param max-rtl-if-conversion-insns=1" } */
/* { dg-final { scan-assembler-not "maskeqz" } } */
/* { dg-final { scan-assembler-not "masknez" } } */

/* The relevant optimization is currently only based on noce_try_cmove_arith,
   so it bypasses noce_convert_multiple_sets by
   --param max-rtl-if-conversion-insns=1 to execute noce_try_cmove_arith.  */

extern long lm, ln, lr;

void
test_ge ()
{
  if (lm >= ln)
    lr += ((long)1 << 32);
  lr += lm;
}

void
test_ltz ()
{
  if (lm < 0)
    lr |= (1 << 16);
  lr += lm;
}

void
test_lez ()
{
  if (lm <= 0)
    lr &= (1 << 16);
  lr += lm;
}

void
test_gez ()
{
  if (lm >= 0)
    lr ^= (1 << 16);
  lr += lm;
}
