/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify there is no ICE on ilp32 env.  */

extern void foo (char *);

void
bar (void)
{
  char zj[] = "XXXXXXXXXXXXXXXX";
  foo (zj);
}
