/* PR tree-optimization/46620 */
/* SRA bitfield grouping used to lose track at padding bitfields in
   the middle of a word.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdlib.h>

struct PCT
{
  unsigned char	pi1	: 4;
  unsigned char	pi2	: 3;
  unsigned char	pif	: 5;

  unsigned char	sl	: 2;
  unsigned char	uc	: 1;
  unsigned char	st	: 1;

  unsigned char	p	: 1;
  unsigned char	cs	: 1;
  unsigned char	ss	: 1;

  unsigned char	pc	: 3;
  unsigned char	dmv	: 4;
  unsigned char	cv	: 4;
};

struct rt
{
  struct rt*		d;
  void (*edo)(void * const);
  short			lId;
  char          dac;
};

struct pedr
{
  struct rt re;
  struct PCT pc;
  unsigned char mid;
} ;

void __attribute__((__noinline__))
rei(struct rt* const me, unsigned short anId, void *ad )
{
  asm volatile ("");
}

void __attribute__((__noinline__))
pedrdo(void * const p)
{
  asm volatile ("");
}

void __attribute__((__noinline__))
pedri (struct pedr* const  me, struct PCT ppc, unsigned char pmid)
{
  rei(&(me->re), 0x7604, 0);
  me->pc = ppc;
  me->mid = pmid;
  (me)->re.edo = pedrdo;
}

int main()
{
  struct PCT ps;
  struct pedr pm;

  pm.pc.dmv = 0;
  ps.dmv = 1;
  pedri(&pm, ps, 32);

  if (pm.pc.dmv != 1)
    abort ();
  exit (0);
}
