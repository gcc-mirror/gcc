/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "maskeqz" } } */
/* { dg-final { scan-assembler-not "masknez" } } */

extern long lm, ln, lr;

void
test_ne ()
{
  if (lm != ln)
    lr += (1 << 16);
  lr += lm;
}

void
test_eq ()
{
  if (lm == ln)
    lr = lm + (1 << 16);
  else
    lr = lm;
  lr += lm;
}

void
test_lt ()
{
  if (lm < ln)
    lr *= (1 << 16);
  lr += lm;
}

void
test_le ()
{
  if (lm <= ln)
    lr = lm * ((long)1 << 32);
  else
    lr = lm;
  lr += lm;
}

void
test_nez ()
{
  if (lm != 0)
    lr <<= (1 << 4);
  lr += lm;
}

void
test_eqz ()
{
  if (lm == 0)
    lr >>= (1 << 2);
  lr += lm;
}
