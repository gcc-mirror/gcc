/* Check for various valid and erroneous "noreturn" cases. */
/* { dg-do compile } */
/* { dg-options "-O2 -Wmissing-noreturn" } */

extern void foo1(void) __attribute__ ((__noreturn__));
void
foo1(void)
{
} /* { dg-warning "`noreturn' function does return" "detect falling off end of noreturn" } */

extern void foo2(void) __attribute__ ((__noreturn__));
void
foo2(void)
{
  exit(0);
} /* { dg-bogus "warning:" "this function should not get any warnings" } */

extern void foo3(void);
void
foo3(void)
{
} /* { dg-bogus "warning:" "this function should not get any warnings" } */

extern void foo4(void);
void
foo4(void)
{
  exit(0);
} /* { dg-warning "candidate for attribute `noreturn'" "detect noreturn candidate" } */

extern void foo5(void) __attribute__ ((__noreturn__));
void
foo5(void)
{
  return; /* { dg-warning "`noreturn' has a `return' statement" "detect invalid return" } */
} /* { dg-warning "`noreturn' function does return" "detect return from noreturn" } */

extern void foo6(void);
void
foo6(void)
{
  return;
} /* { dg-bogus "warning:" "this function should not get any warnings" } */

extern void foo7(void);
void
foo7(void)
{
  foo6();
} /* { dg-bogus "warning:" "this function should not get any warnings" } */

extern void foo8(void) __attribute__ ((__noreturn__));
void
foo8(void)
{
  foo7();
} /* { dg-warning "`noreturn' function does return" "detect return from tail call" } */
