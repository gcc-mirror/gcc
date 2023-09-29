#include "../../gcc.dg/analyzer/analyzer-decls.h"

void test_1 (int pc)
{
  void *arr[2] = {&&x, &&y};
  
  goto *arr[pc];

x:
  __analyzer_dump_path (); /* { dg-message "path" } */
  __analyzer_eval (pc == 0); /* { dg-warning "TRUE" "true" { xfail *-*-* } .-1 } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  return;

 y:
  __analyzer_dump_path (); /* { dg-message "path" } */
  __analyzer_eval (pc == 1); /* { dg-warning "TRUE" "" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  return;
}

void test_duplicates (int pc)
{
  void *arr[3] = {&&x, &&y, &&x};
  int var = 0;
    
  goto *arr[pc];

 x:
  __analyzer_dump_path (); /* { dg-message "path" } */
  __analyzer_eval (pc == 0); /* { dg-warning "UNKNOWN" } */
  return;

 y:
  __analyzer_dump_path (); /* { dg-message "path" } */
  __analyzer_eval (pc == 1); /* { dg-warning "TRUE" "" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  return;
}

void test_multiple (int pc)
{
  void *arr[2] = {&&x, &&y};
  
  goto *arr[pc];

x:
  __analyzer_dump_path (); /* { dg-message "path" } */
  __analyzer_eval (pc == 0); /* { dg-warning "TRUE" "true" { xfail *-*-* } .-1 } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */

  goto *arr[pc];

 y:
  __analyzer_dump_path (); /* { dg-message "path" } */
  __analyzer_eval (pc == 1); /* { dg-warning "TRUE" "" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */

  goto *arr[pc];
}
