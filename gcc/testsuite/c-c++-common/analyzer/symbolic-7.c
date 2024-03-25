#include "analyzer-decls.h"

extern void maybe_write (int *);

void test_1 (int i)
{
  /* An array with purely concrete bindings.  */
  int arr[2];
  arr[0] = 1066;
  arr[1] = 1776;

  /* Concrete reads.  */
  __analyzer_eval (arr[0] == 1066); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[1] == 1776); /* { dg-warning "TRUE" } */

  /* Symbolic read.  */
  __analyzer_describe (0, arr[i]); /* { dg-warning "svalue: 'UNKNOWN\\(int\\)'" } */
  __analyzer_eval (arr[i] == 1776); /* { dg-warning "UNKNOWN" } */
}

void test_2 (int i)
{
  /* An array that could have been touched.  */
  int arr[2];
  maybe_write (arr);
  
  /* Concrete reads.  */
  __analyzer_eval (arr[0] == 42); /* { dg-warning "UNKNOWN" } */

  /* Symbolic read.  */
  __analyzer_eval (arr[i] == 42); /* { dg-warning "UNKNOWN" } */
}

void test_3_concrete_read (int i)
{
  /* An array that can't have been touched.  */
  int arr[2];
  
  /* Concrete reads.  */
  __analyzer_eval (arr[0] == 42); /* { dg-warning "use of uninitialized value 'arr\\\[0\\\]'" } */
}

void test_3_symbolic_read (int i)
{
  /* An array that can't have been touched.  */
  int arr[2];
  
  /* Symbolic read.  */
  __analyzer_eval (arr[i] == 42); /* { dg-warning "use of uninitialized value 'arr\\\[i\\\]'" } */
}
