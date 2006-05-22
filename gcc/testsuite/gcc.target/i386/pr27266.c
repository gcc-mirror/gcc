/* PR target/27266.
   The testcase below used to trigger an ICE.  */

/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-march=pentium" } */

signed long long sll;

void
foo (void)
{
  __sync_fetch_and_add (&sll, 1);
}
