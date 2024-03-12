/* { dg-do compile } */
/* { dg-options "" } */

/* The purpose of this test is to make sure that registers r7, r8 and r9 are
   available for register allocation.  r9 used to be reserved as a stack
   pointer but not any longer.  */

int bar ()
{
  int a, b, c = 10;

  asm volatile ("#lala %[a] %[c]"
		:
		: [a]"r"(&a),
		  [b]"r"(&b),
		  [c]"r"(c)
		: "r0", "r1", "r2", "r3", "r4", "r5", "memory", "r6"
		);

  return a + b + c;
}
