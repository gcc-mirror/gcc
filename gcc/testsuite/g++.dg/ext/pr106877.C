// PR target/106877
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O1 -m16 -mtune=sandybridge -flive-range-shrinkage -fno-dce" }

void
foo (float b, double c)
{
  for (int e = 0; e < 2; e++)
    {
      asm volatile ("" : "+f" (c));	// { dg-error "must specify a single register" }
      asm ("" : "+rm" (c = b));
    }
}
