/* Test C23 alignment support.  Test valid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stddef.h>

alignas (alignof (max_align_t)) char c;
extern alignas (max_align_t) char c;
extern char c;

extern alignas (max_align_t) short s;
alignas (max_align_t) short s;

alignas (int) int i;
extern int i;

alignas (max_align_t) long l;

alignas (max_align_t) long long ll;

alignas (max_align_t) float f;

alignas (max_align_t) double d;

alignas (max_align_t) _Complex long double cld;

alignas (0) alignas (int) alignas (char) char ca[10];

alignas ((int) alignof (max_align_t) + 0) int x;

enum e { E = alignof (max_align_t) };
alignas (E) int y;

void
func (void)
{
  alignas (max_align_t) long long auto_ll;
}

/* Valid, but useless.  */
alignas (0) struct s; /* { dg-warning "useless" } */
