/* Portable assumptions */
/* { dg-do compile } */
/* { dg-options "-std=c23" } */

[[gnu::__assume__ (1)]] void f1 (void);	/* { dg-warning "'assume' attribute not followed by ';'" } */
					/* { dg-warning "'assume' attribute ignored" "" { target *-*-* } .-1 } */
typedef int intx [[gnu::assume (1)]];	/* { dg-warning "'assume' attribute ignored" } */
[[__gnu__::assume (1)]];		/* { dg-warning "'assume' attribute at top level" } */
__attribute__((assume (1))) void f1b ();/* { dg-warning "'assume' attribute not followed by ';'" } */
					/* { dg-warning "'assume' attribute ignored" "" { target *-*-* } .-1 } */
typedef int inty __attribute__((assume (1)));	/* { dg-warning "'assume' attribute ignored" } */

void
foo ()
{
  int i;
  [[gnu::assume]];			/* { dg-error "wrong number of arguments specified for 'assume' attribute" } */
					/* { dg-message "expected 1, found 0" "" { target *-*-* } .-1 } */
  [[gnu::__assume__ ()]];		/* { dg-error "parentheses must be omitted if attribute argument list is empty" } */
					/* { dg-error "wrong number of arguments specified for 'assume' attribute" "" { target *-*-* } .-1 } */
					/* { dg-message "expected 1, found 0" "" { target *-*-* } .-2 } */
  [[gnu::assume (1, 1)]];		/* { dg-error "wrong number of arguments specified for 'assume' attribute" } */
					/* { dg-message "expected 1, found 2" "" { target *-*-* } .-1 } */
  [[gnu::assume (1)]] i = 1;		/* { dg-warning "'assume' attribute ignored" } */
  [[gnu::assume (i = 1)]];		/* { dg-error "expected" } */
					/* { dg-warning "'assume' attribute ignored" "" { target *-*-* } .-1 } */
  __attribute__((assume));		/* { dg-error "wrong number of arguments specified for 'assume' attribute" } */
					/* { dg-message "expected 1, found 0" "" { target *-*-* } .-1 } */
  __attribute__((assume ()));		/* { dg-error "wrong number of arguments specified for 'assume' attribute" } */
					/* { dg-message "expected 1, found 0" "" { target *-*-* } .-1 } */
  __attribute__((assume (1, 1)));	/* { dg-error "wrong number of arguments specified for 'assume' attribute" } */
					/* { dg-message "expected 1, found 2" "" { target *-*-* } .-1 } */
  __attribute__((assume (i = 1)));	/* { dg-error "expected" } */
}

int
f2 (int x)
{
  __asm ("" : "+r" (x));
  return x;
}

int
f3 (int x)
{
  [[gnu::assume (f2 (42) == 42)]];
  return x;
}

int
f3a (int x)
{
  __attribute__((assume (f2 (42) == 42)));
  return x;
}

struct S {};

int
f4 ()
{
  struct S s;
  [[gnu::assume (s)]];			/* { dg-error "used struct type value where scalar is required" } */
  __attribute__((assume (s)));		/* { dg-error "used struct type value where scalar is required" } */
  return 0;
}
