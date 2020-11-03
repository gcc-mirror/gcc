/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC -frename-registers --param=sched-autopref-queue-depth=0 -mdejagnu-cpu=603" } */

extern void bar (void *);

void
foo (void)
{
  bar ("");
}
