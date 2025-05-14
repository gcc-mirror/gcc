/* { dg-additional-options "-fdiagnostics-text-art-charset=unicode" } */

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

void test6 (size_t size)
{
  int32_t *buf = (int32_t *) __builtin_alloca (4 * size);
  memset (buf, 0, 4 * size);
  int32_t last = *(buf + 4 * size); /* { dg-warning "stack-based buffer over-read" } */
}

/* (size * 16) - (size * 4) leads to a gap of (size * 12).  */

/* { dg-begin-multiline-output "" }

                                                       ┌─────────────────┐
                                                       │ read of 4 bytes │
                                                       └─────────────────┘
                                                                ^
                                                                │
                                                                │
  ┌────────────────────────────────┐                   ┌─────────────────┐
  │buffer allocated on stack at (1)│                   │after valid range│
  └────────────────────────────────┘                   └─────────────────┘
  ├───────────────┬────────────────┤├────────┬────────┤├────────┬────────┤
                  │                          │                  │
                  │                          │       ╭──────────┴─────────╮
                  │                          │       │over-read of 4 bytes│
                  │                          │       ╰────────────────────╯
                  │         ╭────────────────┴───────────────╮
                  │         │'(size * 16) - (size * 4)' bytes│
                  │         ╰────────────────────────────────╯
      ╭───────────┴──────────╮
      │size: 'size * 4' bytes│
      ╰──────────────────────╯

   { dg-end-multiline-output "" } */

void test7 (size_t size)
{
  int32_t *buf = (int32_t *) __builtin_alloca (4 * size + 3); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
  buf[size] = 42; /* { dg-warning "stack-based buffer overflow" } */
}

/* { dg-begin-multiline-output "" }

                                ┌────────────────────────────────────────┐
                                │        write of '(int32_t) 42'         │
                                └────────────────────────────────────────┘
                                         │                     │
                                         │                     │
                                         v                     v
  ┌────────────────────────────────────────────────┐ ┌───────────────────┐
  │        buffer allocated on stack at (1)        │ │ after valid range │
  └────────────────────────────────────────────────┘ └───────────────────┘
  ├───────────────────────┬────────────────────────┤ ├─────────┬─────────┤
                          │                                    │
         ╭────────────────┴───────────────╮          ╭─────────┴────────╮
         │capacity: '(size * 4) + 3' bytes│          │overflow of 1 byte│
         ╰────────────────────────────────╯          ╰──────────────────╯

   { dg-end-multiline-output "" } */


/* We're currently not able to generate a diagram for this case;
   make sure we handle this case gracefully.  */

char *test99 (const char *x, const char *y)
{
  size_t len_x = __builtin_strlen (x);
  size_t len_y = __builtin_strlen (y);
  /* BUG (root cause): forgot to add 1 for terminator.  */
  size_t sz = len_x + len_y;
  char *result = (char *) __builtin_malloc (sz);
  if (!result)
    return NULL;
  __builtin_memcpy (result, x, len_x);
  __builtin_memcpy (result + len_x, y, len_y);
  /* BUG (symptom): off-by-one out-of-bounds write to heap.  */
  result[len_x + len_y] = '\0'; /* { dg-warning "heap-based buffer overflow" } */
  return result;
}
