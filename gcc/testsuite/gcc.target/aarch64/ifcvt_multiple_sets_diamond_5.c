/* Test a diamond whose likely fallthrough arm has one set and whose other
   arm has multiple sets, including a live-out not changed by the fallthrough
   arm.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */
/* { dg-additional-options "--param=max-rtl-if-conversion-predictable-cost=100" } */

unsigned long
f (const unsigned char *p, unsigned long tag)
{
  unsigned long type = tag & 3;
  unsigned long next = type;
  unsigned long advance;
  if (__builtin_expect (type != 0, 1))
    advance = type + 1;
  else
    {
      unsigned long base = tag >> 2;
      next = base + 1;
      advance = base + 2;
    }
  return p[next] + (advance << 8);
}

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
/* { dg-final { scan-assembler-times "\tcsinc\t" 1 } } */
/* { dg-final { scan-assembler-times "\tcsel\t" 1 } } */
/* { dg-final { scan-assembler-times "\tldrb\t" 1 } } */
/* { dg-final { scan-assembler-not {\tb(eq|ne)\t} } } */
/* { dg-final { scan-assembler-not {\tcbn?z\t} } } */
/* { dg-final { scan-assembler-not {\ttbn?z\t} } } */
