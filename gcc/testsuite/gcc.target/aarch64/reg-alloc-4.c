/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#define PROB 0.1

struct L
{
  int data;
  volatile struct L *next;
  volatile struct L *inner;
};

/* The thing we're testing here is that the !head->inner path of the outer loop
   body has no stack accesses.  It's possible that we'll need to update this
   pattern for unrelated code changes. but the test should be XFAILed rather
   than changed if any new stack accesses occur on the !head->inner path.  */
/*
** foo:
**	...
**	ldr	(w[0-9]+), \[(x[0-9]+)\]
**	add	(w[0-9]+), (?:\3, \1|\1, \3)
**	ldr	(x[0-9]+), \[\2, #?16\]
**	str	\3, \[\2\]
**	ldr	\2, \[\2, #?8\]
**	cbn?z	\4, .*
**	...
**	ret
*/
void
foo (volatile struct L *head, int inc)
{
  while (head)
    {
      /* Clobber all call-preserved GPRs, so that the loop has to use
	 call-clobbered GPRs if it is to avoid spilling.  */
      asm volatile ("" :::
		    "x19", "x20", "x21", "x22", "x23",
		    "x24", "x25", "x26", "x27", "x28");
      inc = head->data + inc;
      volatile struct L *inner = head->inner;
      head->data = inc;
      head = head->next;
      if (__builtin_expect_with_probability (inner != 0, 0, PROB))
	for (int i = 0; i < 1000; ++i)
	  asm volatile ("" ::			/* example allocation: */
			"r" (i),		/* x0 */
			"r" (inner),		/* x1 */
			"r" (inner->next),	/* x2 */
			"r" (inner->next),	/* x3 */
			"r" (inner->next),	/* x4 */
			"r" (inner->next),	/* x5 */
			"r" (inner->next),	/* x6 */
			"r" (inner->next),	/* x7 */
			"r" (inner->next),	/* x8 */
			"r" (inner->next),	/* x9 */
			"r" (inner->next),	/* x10 */
			"r" (inner->next),	/* x11 */
			"r" (inner->next),	/* x12 */
			"r" (inner->next),	/* x13 */
			"r" (inner->next),	/* x14 */
			"r" (inner->next),	/* x15 */
			"r" (inner->next),	/* x16 */
			"r" (inner->next),	/* x17 */
			"r" (inner->next),	/* x18 */
			"r" (inner->next) :	/* x30 */
			"x19", "x20", "x21", "x22", "x23",
			"x24", "x25", "x26", "x27", "x28");
    }
}
