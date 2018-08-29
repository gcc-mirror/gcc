// PR inline-asm/84683
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2" }

void
foo (float b, double c)
{
  for (int e = 0; e < 2; e++)
    {
      asm volatile ("" : "+f" (c));	// { dg-error "must specify a single register" }
      asm ("" : "+rm" (c = b));
    }
}
