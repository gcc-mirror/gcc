/* Test C11 alignment support.  Test valid code using stdalign.h.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdalign.h>
#include <stddef.h>

extern int strcmp (const char *, const char *);

extern void exit (int);
extern void abort (void);

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

#ifndef alignas
#error "alignas not defined"
#endif

#ifndef alignof
#error "alignof not defined"
#endif

#ifndef __alignas_is_defined
#error "__alignas_is_defined not defined"
#endif

#if __alignas_is_defined != 1
#error "__alignas_is_defined not 1"
#endif

#ifndef __alignof_is_defined
#error "__alignof_is_defined not defined"
#endif

#if __alignof_is_defined != 1
#error "__alignof_is_defined not 1"
#endif

#define str(x) #x
#define xstr(x) str(x)

const char *s1 = xstr(alignas);
const char *s2 = xstr(alignof);
const char *s3 = xstr(__alignas_is_defined);
const char *s4 = xstr(__alignof_is_defined);

int
main (void)
{
  if (strcmp (s1, "_Alignas") != 0)
    abort ();
  if (strcmp (s2, "_Alignof") != 0)
    abort ();
  if (strcmp (s3, "1") != 0)
    abort ();
  if (strcmp (s4, "1") != 0)
    abort ();
}
