/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-F." } */

/* The intent of the test is to show that we find a framework that
   is installed in /System/Library/Frameworks when the user has added
   a '-F' option.  The trick is to choose some header that is present
   for all the Darwin versions we support and that does not contain any
   content we cannot parse.  */

#include <IOKit/IOReturn.h>
