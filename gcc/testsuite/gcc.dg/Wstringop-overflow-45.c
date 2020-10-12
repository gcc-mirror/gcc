/* PR middle-end/97023 - missing warning on buffer overflow in chained mempcpy
   Verify that out of bounds writes by built-ins to objects through pointers
   returned by other built-ins are diagnosed.
   { dg-do compile }
   { dg-options "-O2" } */

#include "range.h"

void* malloc (size_t);
void* memcpy (void*, const void*, size_t);
void* memmove (void*, const void*, size_t);
void* mempcpy (void*, const void*, size_t);

void sink (void*, ...);


void nowarn_memcpy (const void *s)
{
  extern char cpy_a4[4];
  unsigned n = sizeof cpy_a4;

  void *p = cpy_a4;
  p = memcpy (p, s, n);
  sink (p);
  memcpy (p, s, n);
  sink (p);

  p = cpy_a4 + 1;
  p = memcpy (p, s, n - 1);
  sink (p);
  memcpy (p, s, n - 1);
  sink (p);

  p = cpy_a4 + 2;
  p = memcpy (p, s, n - 2);
  sink (p);
  memcpy (p, s, n - 2);
  sink (p);

  p = cpy_a4 + 3;
  p = memcpy (p, s, n - 3);
  sink (p);
  memcpy (p, s, n - 3);
  sink (p);

  p = cpy_a4 + 4;
  p = memcpy (p, s, n - 4);
  sink (p);
  memcpy (p, s, n - 4);
  sink (p);
}


void nowarn_memcpy_chain (const void *s)
{
  extern char cpy_a8[8];

  char *p = cpy_a8;

  p = memcpy (p + 1, s, 7);
  sink (p);

  p = memcpy (p + 2 , s, 5);
  sink (p);

  p = memcpy (p + 3 , s, 2);
  sink (p);

  p = memcpy (p + 1 , s, 1);
  sink (p);

  p = memcpy (p - 7 , s, 8);
  sink (p);

  memcpy (p + 1, s, 7);
}


void warn_memcpy (const void *s)
{
  extern char cpy_a5[5];                // { dg-message "destination object 'cpy_a5'" "note" }

  unsigned n = sizeof cpy_a5;
  void *p = cpy_a5;

  p = memcpy (p, s, n);
  sink (p);
  memcpy (p, s, n + 1);                 // { dg-warning "writing 6 bytes into a region of size 5" }
  sink (p);

  p = cpy_a5;
  p = memcpy (p, s, n);
  sink (p);
  memcpy (p, s, n + 1);                 // { dg-warning "writing 6 bytes into a region of size 5" }
  sink (p);

  p = cpy_a5 + 1;
  p = memcpy (p, s, n - 1);
  sink (p);
  memcpy (p, s, n);                     // { dg-warning "writing 5 bytes into a region of size 4" }
  sink (p);
}


void warn_memcpy_chain (const void *s)
{
  extern char cpy_a8[8];                // { dg-message "destination object 'cpy_a8'" "note" }

  char *p = cpy_a8;

  p = memcpy (p, s, 9);                 // { dg-warning "writing 9 bytes into a region of size 8" }
  sink (p);

  p = memcpy (p + 2, s, 7);             // { dg-warning "writing 7 bytes into a region of size 6" }
  sink (p);

  p = memcpy (p + 3, s, 5);             // { dg-warning "writing 5 bytes into a region of size 3" }
  sink (p);

  p = memcpy (p + 3, s, 3);             // { dg-warning "writing 3 bytes into a region of size 0" }
  sink (p);
}


void nowarn_mempcpy (const void *s)
{
  extern char a4[4];
  unsigned n = sizeof a4;

  char *p = mempcpy (a4, s, n);
  sink (p);
  mempcpy (p - 4, s, n);
  sink (p);

  p = mempcpy (a4 + 1, s, n - 1);
  sink (p);
  mempcpy (p - 4, s, n);
  sink (p);

  p = mempcpy (a4 + 2, s, n - 2);
  sink (p);
  mempcpy (p - 4, s, n);
  sink (p);

  p = mempcpy (a4 + 3, s, n - 3);
  sink (p);
  mempcpy (p - 4, s, n);
  sink (p);

  p = mempcpy (a4 + 4, s, n - 4);
  sink (p);
  mempcpy (p - 4, s, n);
  sink (p);
}


void nowarn_mempcpy_chain (const void *s)
{
  extern char pcpy_a8[8];

  char *p = pcpy_a8;

  p = mempcpy (p + 1, s, 7);
  sink (p);

  p = mempcpy (p - 7 , s, 7);
  sink (p);

  p = mempcpy (p - 5 , s, 5);
  sink (p);

  p = mempcpy (p - 3 , s, 3);
  sink (p);

  p = mempcpy (p - 2 , s, 2);
  sink (p);

  mempcpy (p - 1, s, 1);
  sink (p);

  mempcpy (p - 8, s, 8);
}


void warn_mempcpy (const void *s)
{
  extern char pcpy_a5[5];               // { dg-message "destination object 'pcpy_a5'" "note" }

  char *p = pcpy_a5;

  p = mempcpy (p, s, 5);
  sink (p);
  mempcpy (p - 5, s, 6);                // { dg-warning "writing 6 bytes into a region of size 5 " }
  sink (p);

  p = pcpy_a5;
  p = mempcpy (p, s, 3);
  sink (p);
  mempcpy (p, s, 3);                    // { dg-warning "writing 3 bytes into a region of size 2 " }
  sink (p);

  p = pcpy_a5 + 1;
  p = mempcpy (p, s, 3);
  sink (p);
  mempcpy (p - 1, s, 5);                // { dg-warning "writing 5 bytes into a region of size 2 " }
  sink (p);
}


void warn_mempcpy_chain_3 (const void *s)
{
  char *p = malloc (5);                 // { dg-message "at offset \\\[3, 5] into destination object of size 5" "note" }
  p = mempcpy (p, s, UR (1, 2));
  p = mempcpy (p, s, UR (2, 3));
  p = mempcpy (p, s, UR (3, 4));        // { dg-warning "writing between 3 and 4 bytes into a region of size 2 " }

  sink (p);
}

void warn_mempcpy_offrng_chain_3 (const void *s)
{
  char *p = malloc (11);                 // { dg-message "at offset \\\[9, 11] into destination object of size 11 " "note" }
  size_t r1_2 = UR (1, 2);
  size_t r2_3 = r1_2 + 1;
  size_t r3_4 = r2_3 + 1;

  p = mempcpy (p + r1_2, s, r1_2);
  p = mempcpy (p + r2_3, s, r2_3);
  p = mempcpy (p + r3_4, s, r3_4);       // { dg-warning "writing between 3 and 4 bytes into a region of size 2 " }

  sink (p);
}

void warn_mempcpy_chain_4 (const void *s)
{
  char *p = malloc (9);                 // { dg-message "at offset \\\[6, 9] into destination object of size 9 " "note" }
  p = mempcpy (p, s, UR (1, 2));
  p = mempcpy (p, s, UR (2, 3));
  p = mempcpy (p, s, UR (3, 4));
  p = mempcpy (p, s, UR (4, 5));        // { dg-warning "writing between 4 and 5 bytes into a region of size 3 " }

  sink (p);
}

void warn_mempcpy_chain_5 (const void *s)
{
  char *p = malloc (14);                // { dg-message "at offset \\\[10, 14] into destination object of size 14 " "note" }
  p = mempcpy (p, s, UR (1, 2));
  p = mempcpy (p, s, UR (2, 3));
  p = mempcpy (p, s, UR (3, 4));
  p = mempcpy (p, s, UR (4, 5));
  p = mempcpy (p, s, UR (5, 6));        // { dg-warning "writing between 5 and 6 bytes into a region of size 4 " }

  sink (p);
}
