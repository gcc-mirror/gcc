/* PR target/6838 from cato@df.lth.se.
   cris-elf got an ICE with -O2: the insn matching
      (insn 49 48 52 (parallel[ 
		  (set (mem/s:HI (plus:SI (reg/v/f:SI 0 r0 [24])
			      (const_int 8 [0x8])) [5 <variable>.c+0 S2 A8])
		      (reg:HI 2 r2 [27]))
		  (set (reg/f:SI 2 r2 [31])
		      (plus:SI (reg/v/f:SI 0 r0 [24])
			  (const_int 8 [0x8])))
	      ] ) 24 {*mov_sidehi_mem} (nil)
	  (nil))
   forced a splitter through the output pattern "#", but there was no
   matching splitter.  */

struct xx
 {
   int a;
   struct xx *b;
   short c;
 };

int f1 (struct xx *);
void f2 (void);

int
foo (struct xx *p, int b, int c, int d)
{
  int a;

  for (;;)
    {
      a = f1(p);
      if (a)
	return (0);
      if (b)
	continue;
      p->c = d;
      if (p->a)
	f2 ();
      if (c)
	f2 ();
      d = p->c;
      switch (a)
	{
	case 1:
	  if (p->b)
	    f2 ();
	  if (c)
	    f2 ();
	default:
	  break;
	}
    }
  return d;
}

int main (void)
{
  struct xx s = {0, &s, 23};
  if (foo (&s, 0, 0, 0) != 0 || s.a != 0 || s.b != &s || s.c != 0)
    abort ();
  exit (0);
}

int
f1 (struct xx *p)
{
  static int beenhere = 0;
  if (beenhere++ > 1)
    abort ();
  return beenhere > 1;
}

void
f2 (void)
{
  abort ();
}
