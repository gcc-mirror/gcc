/* PR c/67730 */
/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

#include "pr67730.h"

extern void bar (unsigned char *);

unsigned char *
f (void *p)
{
   unsigned char *uc = ONEP; /* { dg-warning "request for implicit conversion" } */
   uc = ONEP; /* { dg-warning "request for implicit conversion" } */
   bar (ONEP); /* { dg-warning "request for implicit conversion" } */
   return ONEP; /* { dg-warning "request for implicit conversion" } */
}
