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
      inc = head->data + inc;
      volatile struct L *inner = head->inner;
      head->data = inc;
      head = head->next;
      if (__builtin_expect_with_probability (inner != 0, 0, PROB))
	for (int i = 0; i < 1000; ++i)
	  /* Leave x30 for i.  */
	  asm volatile ("// foo" :::
			"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
			"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
			"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
			"x24", "x25", "x26", "x27", "x28");
    }
}
