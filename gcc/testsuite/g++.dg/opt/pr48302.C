// { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } }
// { dg-options "-O -fcrossjumping -fgcse -fschedule-insns2 -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops -fselective-scheduling2 --param lim-expensive=320 --param min-crossjump-insns=1" }

struct S
{
  int i, j;
  S *s;
  ~S();
};

S *gs;

void foo ()
{
restart:
  for (S *s = gs; s; s = s->s)
    if (s->j && s->j != (s->i & 1)) {
	    delete s;
	    goto restart;
      }
}
