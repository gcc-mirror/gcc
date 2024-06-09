/* { dg-do compile } */
/* { dg-options "-O3 -Wno-stringop-overflow -fstack-protector-strong -mdejagnu-cpu=power8" } */
/* { dg-require-effective-target powerpc_vsx } */

/* PR93658: Failure compiling this test is an infinite loop in LRA's
   process_address(), so set a short timeout limit.  */
/* { dg-timeout 5 } */

void bar();
char b;
void
foo (void)
{
  char a;
  int d = b;
  char *e = &a;
  while (d)
    *e++ = --d;
  bar ();
}
