/* { dg-do compile } */
/* { dg-options "-O -fwhole-program" } */

static void foo1 (void) { }					/* { dg-warning "have effect only on public" } */
extern void foo1 (void) __attribute__((externally_visible));

void foo2 (void)
{
  __attribute__((externally_visible)) void foo3 (void) { }	/* { dg-warning "have effect only on public" } */
}

__attribute__((externally_visible)) static void foo3 (void) { }	/* { dg-warning "have effect only on public" } */

static int bar1;
extern int bar1 __attribute__((externally_visible));		/* { dg-warning "have effect only on public" } */

static int bar2 __attribute__((externally_visible));		/* { dg-warning "have effect only on public" } */

void fn1 (void)
{
  static int bar3 __attribute__((externally_visible));		/* { dg-warning "have effect only on public" } */
}

void fn2 (void)
{
  int bar4 __attribute__((externally_visible));			/* { dg-warning "have effect only on public" } */
}

struct A
{
} __attribute__((externally_visible));				/* { dg-warning "does not apply to types" } */

typedef int B __attribute__((externally_visible));		/* { dg-warning "attribute ignored" } */
