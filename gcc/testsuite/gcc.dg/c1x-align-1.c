/* Test C1X alignment support.  Test valid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

#include <stddef.h>

_Alignas (_Alignof (max_align_t)) char c;
extern _Alignas (max_align_t) char c;
extern char c;

extern _Alignas (max_align_t) short s;
_Alignas (max_align_t) short s;

_Alignas (int) int i;
extern int i;

_Alignas (max_align_t) long l;

_Alignas (max_align_t) long long ll;

_Alignas (max_align_t) float f;

_Alignas (max_align_t) double d;

_Alignas (max_align_t) _Complex long double cld;

_Alignas (0) _Alignas (int) _Alignas (char) char ca[10];

_Alignas ((int) _Alignof (max_align_t) + 0) int x;

enum e { E = _Alignof (max_align_t) };
_Alignas (E) int y;

void
func (void)
{
  _Alignas (max_align_t) long long auto_ll;
}

/* Valid, but useless.  */
_Alignas (0) struct s; /* { dg-warning "useless" } */
