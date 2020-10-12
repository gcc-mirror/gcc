#include "analyzer-decls.h"

struct coord
{
  int x;
  int y;
};

/* Copying from one on-stack array to another.  */

void test_1 (void)
{
  struct coord arr_a[16];
  struct coord arr_b[16];
  arr_a[3].x = 5;
  arr_a[3].y = 6;

  arr_b[7] = arr_a[3];

  __analyzer_eval (arr_b[7].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr_b[7].y == 6); /* { dg-warning "TRUE" } */
}

/* Copying from an on-stack array to a global array.  */

struct coord glob_arr[16];

void test_2 (void)
{
  struct coord arr[16];
  arr[3].x = 5;
  arr[3].y = 6;

  glob_arr[7] = arr[3];

  __analyzer_eval (glob_arr[7].x == 5); /* { dg-warning "TRUE" } */
  __analyzer_eval (glob_arr[7].y == 6); /* { dg-warning "TRUE" } */
}

/* Copying from a partially initialized on-stack array to a global array.  */

struct coord glob_arr[16];

void test_3 (void)
{
  struct coord arr[16];
  arr[3].y = 6;

  glob_arr[7] = arr[3]; // or should the uninit warning be here?

  __analyzer_eval (glob_arr[7].x); /* { dg-warning "uninitialized" "uninit" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  __analyzer_eval (glob_arr[7].y == 6); /* { dg-warning "TRUE" } */
}

/* Symbolic bindings: copying from one array to another.  */

struct coord glob_arr[16];

void test_4 (int i)
{
  struct coord arr_a[16];
  struct coord arr_b[16];
  arr_a[i].x = 5;
  arr_a[i].y = 6;
  __analyzer_eval (arr_a[i].x == 5); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
  __analyzer_eval (arr_a[i].y == 6); /* { dg-warning "TRUE" } */

  arr_b[i] = arr_a[i];

  __analyzer_eval (arr_b[i].x == 5); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
  __analyzer_eval (arr_b[i].y == 6); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
}

/* Symbolic bindings: copying within an array: symbolic src and dest  */

struct coord glob_arr[16];

void test_5a (int i, int j)
{
  struct coord arr[16];
  arr[i].x = 5;
  arr[i].y = 6;

  arr[j] = arr[i];

  __analyzer_eval (arr[j].x == 5); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
  __analyzer_eval (arr[j].y == 6); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
}

/* Symbolic bindings: copying within an array: symbolic src, concrete dest.  */

struct coord glob_arr[16];

void test_5b (int i)
{
  struct coord arr[16];
  arr[i].x = 5;
  arr[i].y = 6;

  arr[3] = arr[i];

  __analyzer_eval (arr[3].x == 5); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
  __analyzer_eval (arr[3].y == 6); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
}

/* Symbolic bindings: copying within an array: concrete src, symbolic dest.  */

struct coord glob_arr[16];

void test_5c (int i)
{
  struct coord arr[16];
  arr[3].x = 5;
  arr[3].y = 6;

  arr[i] = arr[3];

  __analyzer_eval (arr[i].x == 5); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
  __analyzer_eval (arr[i].y == 6); /* { dg-warning "TRUE" "TRUE" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "UNKNOWN" { xfail *-*-* } .-1 } */
}

/* No info on the subregion being copied, and hence
   binding_cluster2::maybe_get_compound_binding should return NULL.  */

void test_6 (void)
{
  struct coord arr[16];
  arr[7] = glob_arr[3];

  __analyzer_eval (arr[7].x == 5); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (arr[7].y == 6); /* { dg-warning "UNKNOWN" } */
}
