/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#define PROB 0.1

struct L
{
  int data;
  volatile struct L *next;
  volatile struct L *inner;
};

void ext();

/* The thing we're testing here is that the !head->inner path of the outer loop
   body has no stack accesses.  It's possible that we'll need to update this
   pattern for unrelated code changes. but the test should be XFAILed rather
   than changed if any new stack accesses creep into the !head->inner path.  */
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
foo (volatile struct L *head, int inc, double *ptr)
{
  double d = *ptr;
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
	  {
	    ext ();
	    /* Hack to create high register pressure, so that IRA doesn't
	       collapse this loop into the parent loop.  */
	    d += 1;
	    asm volatile ("// foo" :::
			  "d0", "d1", "d2", "d3",
			  "d4", "d5", "d6", "d7",
			  "d8", "d9", "d10", "d11",
			  "d12", "d13", "d14", "d15",
			  "d16", "d17", "d18", "d19",
			  "d20", "d21", "d22", "d23",
			  "d24", "d25", "d26", "d27",
			  "d28", "d29", "d30", "d31");
	  }
    }
  *ptr = d;
}
