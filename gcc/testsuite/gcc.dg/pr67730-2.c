/* PR c/67730 */
/* { dg-do compile } */
/* { dg-options "" } */

#include "pr67730.h"

extern void bar (int);

int
fn1 (void)
{
  int a = NULL; /* { dg-warning "initialization makes integer from pointer" } */
  a = NULL; /* { dg-warning "assignment makes integer from pointer" } */
  bar (NULL); /* { dg-warning "passing argument 1" } */
  return NULL; /* { dg-warning "return makes integer from pointer" } */
}

int
fn2 (void)
{
  RETURN; /* { dg-warning "return makes integer from pointer" } */
}
