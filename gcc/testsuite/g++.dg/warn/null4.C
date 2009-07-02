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
  // Adding to the NULL pointer, which has no specific type, should
  // result in a warning; the type of the resulting expression is
  // actually "int", not a pointer type.
  if (NULL + 1) return -1;    // { dg-warning "NULL used in arithmetic" }
  if (1 + NULL) return -1;    // { dg-warning "NULL used in arithmetic" }
  return 0;
}

int *ip;

struct S {};
typedef int S::*SPD;
typedef void (S::*SPF)(void);
SPD spd;
SPF spf;

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
  // Subtraction of pointers is vaild, so using NULL is OK.
  if (ip - NULL) return -1;
  if (NULL - NULL) return -1;
  // Comparing NULL with a pointer-to-member is OK.
  if (NULL == spd) return -1;
  if (spd == NULL) return -1;
  if (NULL != spd) return -1;
  if (spd != NULL) return -1;
  if (NULL == spf) return -1;
  if (spf == NULL) return -1;
  if (NULL != spf) return -1;
  if (spf != NULL) return -1;

  return 0;
}
