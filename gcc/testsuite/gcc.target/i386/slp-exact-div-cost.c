/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ftree-slp-vectorize -march=x86-64-v3" } */

/* The scalar cost of an exact division by a power of two is a shift, so
   the two pointer differences of the std::deque size computation are not
   worth vectorizing.  */

struct it { void **cur, **first, **last, ***node; };
struct dq { void ***map; unsigned long map_size; struct it start, finish; };

long
size (const struct dq *d)
{
  return 64 * (d->finish.node - d->start.node - (d->finish.node != 0))
	 + (d->finish.cur - d->finish.first) + (d->start.last - d->start.cur);
}

/* { dg-final { scan-assembler-not "vpsubq" } } */
