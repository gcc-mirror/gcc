/* { dg-do compile  } */
/* { dg-options "-std=c99 -pedantic-errors" } */
/* Radar 4336222 */

int a;
struct s { void (*f)(int (*)[a]); };

static int i;
static int new_i() { i++; return i; }
static int bar1(int a[new_i()][new_i()]);

void foo(int n) {
  extern void bar(int i[n][n]);			/* Since this isn't a VM type ensure we can have linkage.  */
  extern int bar1(int a[new_i()][new_i()]);	/* Since this isn't a VM type ensure we can have linkage.  */
}

void foo1(int n) {
  goto A;
  void bar(int i[n][n]);			/* Not a VM type, as VM arguments don't matter. */
  int bar1(int a[new_i()][new_i()]);		/* Not a VM type, as VM arguments don't matter. */
 A:
  ;
}

void foo2(int n) {
  goto A;		/* { dg-error "jump into scope of identifier with variably modified type" } */
  int (*(*bar2)(void))[n];
 A:
  ;
}
