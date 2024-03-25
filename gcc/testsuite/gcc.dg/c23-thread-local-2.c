/* Test that thread-local declarations are not considered tentative definitions
   in C23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

thread_local int a; /* { dg-message "previous" } */
thread_local int a; /* { dg-error "redefinition" } */

static thread_local int b; /* { dg-message "previous" } */
static thread_local int b; /* { dg-error "redefinition" } */

thread_local int c; /* { dg-message "previous" } */
thread_local int c = 1; /* { dg-error "redefinition" } */

static thread_local int d; /* { dg-message "previous" } */
static thread_local int d = 1; /* { dg-error "redefinition" } */

thread_local int e = 1; /* { dg-message "previous" } */
thread_local int e; /* { dg-error "redefinition" } */

static thread_local int f = 1; /* { dg-message "previous" } */
static thread_local int f; /* { dg-error "redefinition" } */

/* Not being a tentative definition means that incomplete arrays are an error
   rather than defaulting to size 1.  */
thread_local int g[]; /* { dg-error "storage size" } */
static thread_local int h[]; /* { dg-error "array size missing" } */
extern thread_local int i[];

thread_local int j[] = { 0 };
static thread_local int k[] = { 0 };

thread_local int l;
extern thread_local int l;

extern thread_local int m;
thread_local int m;

static thread_local int n;
extern thread_local int n;
