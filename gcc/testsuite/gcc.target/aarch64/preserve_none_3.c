/* { dg-do run } */
/* { dg-options "-O2 -std=gnu23" } */

int no_arg_stack_use_callee
  [[gnu::preserve_none, gnu::noinline,
    gnu::noipa]] (int a0, int a1, int a2, int a3, int a4, int a5, int a6,
		  int a7, int a8, int a9, int a10, int a11, int a12, int a13,
		  int a14, int a15, int a16, int a17, int a18, int a19, int a20,
		  int a21, int a22, int a23)
{
  /* Clobber all the registers to check they are correctly marked live at the
     start.  */
  asm volatile("mov x0, #0;"
	       "mov x1, #0;"
	       "mov x2, #0;"
	       "mov x3, #0;"
	       "mov x4, #0;"
	       "mov x5, #0;"
	       "mov x6, #0;"
	       "mov x7, #0;"
	       "mov x8, #0;"
	       "mov x9, #0;"
	       "mov x10, #0;"
	       "mov x11, #0;"
	       "mov x12, #0;"
	       "mov x13, #0;"
	       "mov x14, #0;"
	       "mov x15, #0;"
	       "mov x16, #0;"
	       "mov x17, #0;"
	       "mov x18, #0;"
	       "mov x19, #0;"
	       "mov x20, #0;"
	       "mov x21, #0;"
	       "mov x22, #0;"
	       "mov x23, #0;"
	       "mov x24, #0;"
	       "mov x25, #0;"
	       "mov x26, #0;"
	       "mov x27, #0;"
	       "mov x28, #0;" ::
		 : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
		   "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17",
		   "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25",
		   "x26", "x27", "x28");

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13
	 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23;
}

int arg_stack_use_callee
  [[gnu::preserve_none, gnu::noinline,
    gnu::noipa]] (int a0, int a1, int a2, int a3, int a4, int a5, int a6,
		  int a7, int a8, int a9, int a10, int a11, int a12, int a13,
		  int a14, int a15, int a16, int a17, int a18, int a19, int a20,
		  int a21, int a22, int a23, int a24)
{
  /* Clobber all the registers to check they are correctly marked live at the
     start.  */
  asm volatile("mov x0, #0;"
	       "mov x1, #0;"
	       "mov x2, #0;"
	       "mov x3, #0;"
	       "mov x4, #0;"
	       "mov x5, #0;"
	       "mov x6, #0;"
	       "mov x7, #0;"
	       "mov x8, #0;"
	       "mov x9, #0;"
	       "mov x10, #0;"
	       "mov x11, #0;"
	       "mov x12, #0;"
	       "mov x13, #0;"
	       "mov x14, #0;"
	       "mov x15, #0;"
	       "mov x16, #0;"
	       "mov x17, #0;"
	       "mov x18, #0;"
	       "mov x19, #0;"
	       "mov x20, #0;"
	       "mov x21, #0;"
	       "mov x22, #0;"
	       "mov x23, #0;"
	       "mov x24, #0;"
	       "mov x25, #0;"
	       "mov x26, #0;"
	       "mov x27, #0;"
	       "mov x28, #0;" ::
		 : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9",
		   "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17",
		   "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25",
		   "x26", "x27", "x28");

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13
	 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24;
}

int
main ()
{
  int res = no_arg_stack_use_callee (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
				     13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

  if (res != 23 * 24 / 2)
    return 1;

  res = arg_stack_use_callee (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
			      15, 16, 17, 18, 19, 20, 21, 22, 23, 24);

  if (res != 24 * 25 / 2)
    return 1;

  return 0;
}
