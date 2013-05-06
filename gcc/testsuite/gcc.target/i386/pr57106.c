/* { dg-do compile } */
/* { dg-options "-O2 -fschedule-insns -funroll-all-loops -fcompare-debug" } */

typedef void block128_f (int *, int);

void
foo (int *out, int *iv, block128_f block)
{
  while (1)
    {
      *out = *out ^ *iv;
      block (out, *out);
      iv = out;
    }
}
