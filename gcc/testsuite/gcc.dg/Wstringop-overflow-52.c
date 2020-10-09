
/* PR middle-end/97023 - missing warning on buffer overflow in chained mempcpy
   Verify that writes by built-in functions to objects through pointers
   returned by ordinary (non-built-int) function are assumed to point to
   the beginning of objects.
   { dg-do compile }
   { dg-options "-O2" } */

#include "range.h"

void* memcpy (void*, const void*, size_t);
void* memset (void*, int, size_t);

void sink (void*, ...);

extern char* arrptr[];
extern char* ptr;
extern char* retptr (void);
struct S { char *p; };
extern struct S retstruct (void);

void nowarn_ptr (void)
{
  {
    void *p = arrptr;
    memset (p - 1, 0, 12345);           // { dg-warning "\\\[-Wstringop-overflow" }
    memset (p,0, 12345);
    memset (p,0, DIFF_MAX - 1);
  }

  {
    char *p = arrptr[0];
    memset (p - 1, 0, 12345);
    memset (p - 12345, 0, 12345);
    memset (p - 1234, 0, DIFF_MAX - 1);
    memset (p - DIFF_MAX + 1, 0, 12345);
  }

  {
    char *p = ptr;
    memset (p - 1, 0, 12345);
    memset (p - 12345, 0, 12345);
    memset (p - 1234, 0, DIFF_MAX - 1);
    memset (p - DIFF_MAX + 1, 0, 12345);
  }

  {
    char *p = retptr ();
    memset (p - 1, 0, 12345);
    memset (p - 12345, 0, 12345);
    memset (p - 1234, 0, DIFF_MAX - 1);
    memset (p - DIFF_MAX + 1, 0, 12345);
  }

  {
    char *p = retstruct ().p;
    memset (p - 1, 0, 12345);
    memset (p - 12345, 0, 12345);
    memset (p - 1234, 0, DIFF_MAX - 1);
    memset (p - DIFF_MAX + 1, 0, 12345);
  }
}
