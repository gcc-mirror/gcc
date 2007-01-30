// PR c++/24745 : warnings for NULL constant.
// { dg-do compile  }
// { dg-options "-Wpointer-arith -Wconversion " }

#include <cstddef>

int foo (void) 
{
  if (NULL == 1) return -1;   // { dg-warning "NULL used in arithmetic" } 
  if (NULL > NULL) return -1; // { dg-warning "NULL used in arithmetic" } 
  if (NULL < NULL) return -1; // { dg-warning "NULL used in arithmetic" } 
  if (NULL >= 0) return -1;   // { dg-warning "NULL used in arithmetic" } 
  if (NULL <= 0) return -1;   // { dg-warning "NULL used in arithmetic" } 
  return 0;
}

int bar (void) 
{
  if (NULL) return -1;
  if (!NULL) return -1;
  if (!NULL == 1) return -1;
  if (NULL || NULL) return -1;
  if (!NULL && NULL) return -1;
  if (NULL == NULL) return -1;
  if (NULL != NULL) return -1;
  if (NULL == 0) return -1;
  if (NULL != 0) return -1;
  return 0;
}
