/* PR optimization/13985 */
/* Copied from gcc.c-torture/compile/930621-1.c */

/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-options "-O3 -mtune=i386" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

#if defined(STACK_SIZE) && (STACK_SIZE < 65536)
# define BYTEMEM_SIZE 10000L
#endif

#ifndef BYTEMEM_SIZE
# define BYTEMEM_SIZE 45000L
#endif

int bytestart[5000 + 1];
unsigned char modtext[400 + 1];
unsigned char bytemem[2][BYTEMEM_SIZE + 1];

long
modlookup (int l)
{
  signed char c;
  long j;
  long k;
  signed char w;
  long p;
  while (p != 0)
    {
      while ((k < bytestart[p + 2]) && (j <= l) && (modtext[j] == bytemem[w][k]))
	{
	  k = k + 1;
	  j = j + 1;
	}
      if (k == bytestart[p + 2])
	if (j > l)
	  c = 1;
	else c = 4;
      else if (j > l)
	c = 3;
      else if (modtext[j] < bytemem[w][k])
	c = 0;
      else c = 2;
    }
}
