/* Test for enumeration as NULL pointer constant.  */
/* PR c++/14644 */
/* { dg-do compile } */

enum { NULL = 0 };

void *p = 0;

void *q = NULL;  // { dg-error "cannot convert" }

