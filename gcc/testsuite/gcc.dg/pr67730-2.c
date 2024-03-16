/* PR c/67730 */
/* { dg-do compile } */
/* { dg-options "-fpermissive" } */

#include "pr67730.h"

extern void bar (int);

int
fn1 (void)
{
  int a = NULL; /* { dg-warning "initialization of 'int' from 'void \\*' makes integer from pointer" } */
  a = NULL; /* { dg-warning "assignment to 'int' from 'void \\*' makes integer from pointer" } */
  bar (NULL); /* { dg-warning "passing argument 1" } */
  return NULL; /* { dg-warning "returning 'void \\*' from a function with return type 'int' makes integer from pointer" } */
}

int
fn2 (void)
{
  RETURN; /* { dg-warning "returning 'void \\*' from a function with return type 'int' makes integer from pointer" } */
}
