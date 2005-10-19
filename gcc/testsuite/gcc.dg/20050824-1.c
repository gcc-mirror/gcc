/* Make sure that the S/390 specific shift_count_operand
   predicate work properly.  */

/* { dg-do compile { target s390*-*-* } } */
/* { dg-options "-O3" } */

unsigned long long
f (unsigned long long a, unsigned long b)
{
  asm ("" : : : 
#ifdef __s390x__
		"r13", "r14",
#endif
		"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", 
                "r8", "r9", "r10", "r11", "r12");

  return a << ((b + 3) & 63);
}

unsigned long long
g (unsigned long long a, char **b , int c, int d, int e, int f)
{
  char buffer [4096];

  *b = &buffer[0];

  return a << ((unsigned long)&f & 63);
}

unsigned long long
h (unsigned long long a, int b, int c, int d, int e, int f)
{
  return a << (((unsigned long)&f + 3));
}
