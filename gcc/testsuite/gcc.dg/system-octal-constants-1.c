/* 
   Origin: Dodji Seketeli <dodji@redhat.com>
   { dg-options "-std=iso9899:1999 -pedantic" } 
   { dg-do compile } 
 */

#include "system-octal-constants-1.h"

int
foo (void)
{
#if OCTAL_INT_CONSTANT_IN_SYSTEM_HEADER /* A octal constant defined
					   in system header.  No
					   warning.  */
  return 23;
#endif
  return 0o1307; /* { dg-warning "'0o' prefixed constants are a C2Y feature or GCC extension" } */
}
