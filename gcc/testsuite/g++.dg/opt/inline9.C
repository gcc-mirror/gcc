// PR c++/17972
// Origin: Michal Ostrowski <mostrows@watson.ibm.com>
// Testcase by Alan Modra <amodra@bigpond.net.au>
// { dg-do run }
// { dg-options "-O" }
// { dg-options "-O -mtune=i686" { target i?86-*-* } }

struct thread_info
{
  short preempt_count;
} x;

static inline struct thread_info *cti (void) __attribute__ ((const));
static inline struct thread_info *cti (void)
{
  return &x;
}

void fn (void) __attribute__ ((noinline));
void fn (void)
{
  ++cti()->preempt_count;
}

int main (void)
{
  fn ();
  return 0;
}
