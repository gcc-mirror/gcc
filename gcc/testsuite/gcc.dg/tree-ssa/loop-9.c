/* Without TARGET_MEM_REFs, dom creates code like
   
   i1 = 4 * i;
   *(p + i1) = i;
   *(p + i1 + 4) = i
   
   causing us to have unnecessary multiplication by 4 in the
   result.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

void foo (int *p)
{
  int i;

  for (i = 0; i < 100; i++)
    {
      p[i] = i;
      p[i + 1] = i;
    }
}

/* { dg-final { scan-assembler-times "lea" 0 { target i?86-*-* x86_64-*-* } } } */
