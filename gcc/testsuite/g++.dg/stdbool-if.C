/* { dg-do compile } */
/* { dg-options -pedantic } */

/* test of 'true' and 'false' in #if.  this is accepted with a pedwarn
   before stdbool.h is included, silently afterward.  */

/* Make sure they're viable keywords.  */
bool a = true;
bool b = false;

#if true		/* { dg-warning "true" "true in #if pedwarn" } */
#else
#error true is false	/* { dg-bogus "true" "true is false" } */
#endif

#if false		/* { dg-warning "false" "false in #if pedwarn" } */
#error false is true	/* { dg-bogus "false" "false is true" } */
#endif

#include <stdbool.h>

/* Must still be viable keywords.  */
bool c = true;
bool d = false;

#if true		/* { dg-bogus "true" "true in #if with stdbool.h" } */
#else
#error true is false	/* { dg-bogus "true" "true is false" } */
#endif

#if false		/* { dg-bogus "false" "false in #if with stdbool.h" } */
#error false is true	/* { dg-bogus "false" "false is true" } */
#endif
