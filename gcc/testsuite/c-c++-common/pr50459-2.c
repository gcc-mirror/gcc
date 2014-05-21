/* PR c/50459 */
/* { dg-do compile { target init_priority } } */
/* { dg-options "-Wall -Wextra" } */

enum { A = 128, B = 1 };
void fn3 (void) __attribute__((constructor (A)));
void fn4 (void) __attribute__((destructor (A)));
