/* { dg-do compile } */
/* { dg-options "-Wshadow -Wsystem-headers" } */

#include "Wshadow-4.h"
extern void index2 (void); /* { dg-message "declaration is here" } */

void foo (double index1,
	  double index2)
{
}

void foo1 (void)
{
  double index1;
  double index2;
}

void foo2 (void)
{
  {
    double index1;
    double index2;
  }
}

void foo3 (void)
{
  void (*index1)(void); /* { dg-warning "shadows" } */
  void (*index2)(void); /* { dg-warning "shadows" } */
}

void foo4 (void)
{
  void index1(void) {}; /* { dg-warning "shadows" } */
  void index2(void) {}; /* { dg-warning "shadows" } */
}

/* { dg-message "declaration is here" "" { target *-*-* } 0 } */
