/* Copyright (C) 2000  Free Software Foundation  */
/* Contributed by Alexandre Oliva <aoliva@redhat.com> */

int
foo () 
{
  while (1)
    {
      int a;
      char b;
      /* gcse should not merge these asm statements, since their
	 output operands have different modes.  */
      __asm__("":"=r" (a)); __asm__("":"=r" (b));
      if (b)
	return a;
    }
}
