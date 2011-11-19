/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void ts(void) __attribute__((transaction_safe));
void tp(void) __attribute__((transaction_pure));
void tc(void) __attribute__((transaction_callable));
void ti(void) __attribute__((transaction_unsafe));
void tm(void) __attribute__((transaction_may_cancel_outer));
void tu(void);
int fc(int) __attribute__((const));

typedef void (*Fs) (void) __attribute__((transaction_safe));
typedef void (*Fc) (void) __attribute__((transaction_callable));
typedef void (*Fi) (void) __attribute__((transaction_unsafe));
typedef void (*Fm) (void) __attribute__((transaction_may_cancel_outer));
extern Fs ps;
extern Fc pc;
extern Fi pi;
extern Fm pm;
extern void (*pu)(void);

int __attribute__((transaction_safe))
foo(void)
{
  int i;

  ts();
  tp();
  tc();			/* { dg-error "unsafe function call" } */
  ti();			/* { dg-error "unsafe function call" } */

  /* ??? Direct function calls without markups are handled later
     than pass_diagnose_tm_blocks, which means we'll exit with
     errors before getting there.  This test moved to safe-3.c.  */
  /* tu(); */

  (*ps)();
  (*pc)();		/* { dg-error "unsafe function call" } */
  (*pi)();		/* { dg-error "unsafe function call" } */
  (*pu)();		/* { dg-error "unsafe function call" } */

  asm("");		/* { dg-error "asm not allowed" } */
  asm("" : "=g"(i));	/* { dg-error "asm not allowed" } */

  return fc(i);
}

int __attribute__((transaction_may_cancel_outer))
bar(void)
{
  int i;

  ts();
  tp();
  tc();			/* { dg-error "unsafe function call" } */
  ti();			/* { dg-error "unsafe function call" } */
  tm();

  (*ps)();
  (*pc)();		/* { dg-error "unsafe function call" } */
  (*pi)();		/* { dg-error "unsafe function call" } */
  (*pm)();
  (*pu)();		/* { dg-error "unsafe function call" } */

  asm("");		/* { dg-error "asm not allowed" } */
  asm("" : "=g"(i));	/* { dg-error "asm not allowed" } */

  return fc(i);
}
