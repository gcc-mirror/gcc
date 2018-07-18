/* Test for various initializer warnings being suppressed by use of
   designated initializers.  */

/* { dg-do compile } */
/* { dg-options "-std=c99 -W -Wall -Wtraditional" } */


struct t
{
  int a;
  int b; /* { dg-message "note: 'b' declared here" } */
  int c;
};

union u
{
  int n;
  float i;
};

struct multilevel
{
   int x;
   struct t t;
   union u u;
   union u v;
   char *f; /* { dg-line multilevel_f } */
};

struct t T0 = { 1 };		/* { dg-warning "missing init" } */

struct t T1 = { .a = 1 };	/* { dg-bogus "(missing|near) init" } */

union u U0 = { 1 };		/* { dg-warning "initialization of union" } */
union u U1 = { .i = 1 };	/* { dg-bogus "initialization of union" } */

struct multilevel M =
{
  12,
  { .b = 3 },			/* { dg-bogus "missing init" } */
  { 4 },			/* { dg-warning "initialization of union" } */
  { .n = 9 },			/* { dg-bogus "initialization of union" } */
  /* "string here" */
};				/* { dg-warning "missing init" } */
/* { dg-message "declared here" "near init" { target *-*-* } multilevel_f } */
