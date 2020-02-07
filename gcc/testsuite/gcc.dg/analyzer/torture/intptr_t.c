/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include <stdlib.h>

typedef __INTPTR_TYPE__ intptr_t;
typedef __UINTPTR_TYPE__ uintptr_t;

void test_1 (void)
{
  intptr_t ip;
  void *p = malloc (1024);
  ip = (intptr_t)p;
  free ((void *)ip);
} /* { dg-bogus "leak" } */

void test_2 (void)
{
  uintptr_t uip;
  void *p = malloc (1024);
  uip = (uintptr_t)p;
  free ((void *)uip);
} /* { dg-bogus "leak" } */

void test_3 (intptr_t ip)
{
  free ((void *)ip); /* { dg-message "first 'free'" } */
  free ((void *)ip); /* { dg-warning "double-'free'" } */
}
