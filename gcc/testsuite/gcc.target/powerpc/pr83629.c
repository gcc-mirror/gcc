/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -fPIC -frename-registers --param=sched-autopref-queue-depth=0 -mcpu=603" } */

extern void bar (void *);

void
foo (void)
{
  bar ("");
}
