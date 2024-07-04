/* PR c/114780 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wformat" } */

#include <stddef.h>

[[gnu::sentinel]] void foo (int, ...);
[[gnu::sentinel]] void bar (...);

void
baz (nullptr_t p)
{
  foo (1, 2, nullptr);
  foo (3, 4, 5, p);
  bar (nullptr);
  bar (p);
  foo (6, 7, 0);	// { dg-warning "missing sentinel in function call" }
  bar (0);		// { dg-warning "missing sentinel in function call" }
  foo (8, 9, NULL);
  bar (NULL);
}
