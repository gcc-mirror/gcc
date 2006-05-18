/* { dg-options "-std=c99 -pedantic-errors" } */

int a[*];			/* { dg-error "not allowed in other than function prototype scope" } */
void foo1() { int a[*]; }	/* { dg-error "not allowed in other than function prototype scope" } */
void foo2() { int a[*]; }	/* { dg-error "not allowed in other than function prototype scope" } */
int foo3(int i)[*];		/* { dg-error "not allowed in other than function prototype scope" } */
void foo4(int o[*][4]) { }	/* { dg-error "not allowed in other than function prototype scope" } */
void foo5(int o[4][*]) { }	/* { dg-error "not allowed in other than function prototype scope" } */

/* [*] can't be used in a type that's not a declaration */
void foo11(int x[sizeof(int (*)[*])]);	/* { dg-error "not allowed in other than a declaration" } */
void foo12(int [*]);		/* { dg-error "not allowed in other than a declaration" } */

extern int n;
int B[100];
void foo10(int m) {
  typedef int (*vla)[m];
  struct tag {
    vla x;			/* { dg-error "a member of a structure or union cannot have a variably modified type" } */
    /* PR c/7948 */
    int (*y)[n];		/* { dg-error "a member of a structure or union cannot have a variably modified type" } */
    int z[n];			/* { dg-error "a member of a structure or union cannot have a variably modified type" } */
  };
  /* PR c/25802 */
  extern int (*r)[m];		/* { dg-error "variably modified type must have no linkage" } */
}
