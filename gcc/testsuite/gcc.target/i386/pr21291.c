/* { dg-do compile } */
/* { dg-options "-O" } */

typedef unsigned long bngdigit;
typedef bngdigit *bng;
typedef unsigned int bngcarry;
typedef unsigned long bngsize;

bngdigit
bng_ia32_mult_sub_digit (bng a, bngsize alen, bng b, bngsize blen, bngdigit d)
{
  bngdigit out, tmp;
  bngcarry carry;
  bngdigit a11;

  alen -= blen;
  out = 0;
  asm (""
       : "+r" (a), "+r" (b), "+mr" (blen), "+mr" (out), "=&r" (tmp)
       : "mr" (d)
       : "eax", "edx");
  if (alen == 0)
    {
      a11 = out;
      goto t;
    }

  a11 = 1;
 t:
  return a11;
}
