/* Origin: PR c/5420 from David Mosberger <davidm@hpl.hp.com>.
   This testcase was miscompiled when tail call optimizing, because a
   compound literal initialization was emitted only in the tail call insn
   chain, not in the normal call insn chain.  */

typedef struct { unsigned short a; } A;

extern void abort (void);
extern void exit (int);

void foo (unsigned int x)
{
  if (x != 0x800 && x != 0x810)
    abort ();
}

int
main (int argc, char **argv)
{
  int i;
  for (i = 0; i < 2; ++i)
    foo (((A) { ((!(i >> 4) ? 8 : 64 + (i >> 4)) << 8) + (i << 4) } ).a);
  exit (0);
}
