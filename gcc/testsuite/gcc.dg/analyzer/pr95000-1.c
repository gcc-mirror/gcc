#include "analyzer-decls.h"

void test_1 (char* x)
{
  char* y=0;
  switch (*x) {
  case 'a': /* { dg-message "to here" } */
    y="foo";
  case 'b':
    if (*x=='a') *y='b'; /* { dg-bogus "dereference of NULL 'y'" "deref of null (PR analyzer/95000)" { xfail *-*-* } } */
    /* { dg-warning "write to string literal" "write to string literal" { target *-*-* } .-1 } */
  }
}

void test_switch_char(char x) {
  switch (x) {
  case 'b':
    __analyzer_eval (x == 'b'); /* { dg-warning "TRUE" "expected" { xfail *-*-* } } */
    /* { dg-bogus "UNKNOWN" "status quo (PR analyzer/95000)" { xfail *-*-* } .-1 } */
  }
}

void test_switch_int(int x) {
  switch (x) {
  case 97:
    __analyzer_eval (x == 97); /* { dg-warning "TRUE" } */
  }
}

void test_if_char(char x) {
  if (x == 'b')
    __analyzer_eval (x == 'b'); /* { dg-warning "TRUE" } */
}

void test_if_int(int x) {
  if (x == 97)
    __analyzer_eval (x == 97); /* { dg-warning "TRUE" } */
}
