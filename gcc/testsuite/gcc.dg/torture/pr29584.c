/* PR middle-end/29584 */
/* { dg-do compile { target { ilp32 || lp64 } } } */

extern void *foo1 (void);
extern void foo2 (void);
extern void foo3 (void *, void *);
extern int foo4 (void);

void
bar (void)
{
  int i;
  void *s;
  for (i = 1; i < 4; i++)
    {
      if (foo4 ())
	foo2 ();
      switch (0x8000000UL + i * 0x400)
	{
	case 0x80000000UL ... 0x80000000UL + 0x3a000000UL - 1:
	  s = 0;
	  break;
	default:
	  s = foo1 ();
	}
      foo3 ((void *) (0x8000000UL + i * 0x400), s);
    }
}
