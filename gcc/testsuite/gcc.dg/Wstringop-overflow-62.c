/* Test for MIN and MAX expressions involving pointers.
  { dg-do compile }
  { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

#include "range.h"

#define INT_MAX __INT_MAX__

#define MIN(x, y) ((x) < (y) ? (x) :  (y))
#define MAX(x, y) ((x) < (y) ? (y) :  (x))

typedef __SIZE_TYPE__ size_t;

void* memset (void*, int, size_t);
#define memset(...) sink (memset (__VA_ARGS__))

void sink (void*, ...);

volatile int cond, vi;
char* volatile ptr;

void test_min (void)
{
  const int i1 = SR (1, INT_MAX);
  const int i2 = SR (2, INT_MAX);

  {
    /* Exercise both pointers pointing to a different unknown object plus
       positive constant offset.  Since PTR is volatile P1 and P2 cannot
       normally be considered to point to the same object.  It can only
       be inferred from the MIN expression.  */
    char *p1 = ptr + 1;
    char *p2 = ptr + 2;

    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, INT_MAX);
    // { dg-warning "writing 2147483647 bytes into a region of size 2147483646" "ilp32" { target ilp32 } .-1 }
    memset (q, 0, DIFF_MAX - 2);
    memset (q, 0, DIFF_MAX);
    // { dg-warning "writing 2147483647 bytes into a region of size 2147483646" "ilp32" { target ilp32 } .-1 }
    // { dg-warning "writing 9223372036854775807 bytes into a region of size 9223372036854775806" "lp64" { target lp64 } .-2 }
  }

  {
    /* Exercise both pointers pointing to a different unknown object plus
       variable offset.  */
    char *p1 = ptr + vi;
    char *p2 = ptr + vi;

    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, INT_MAX);
  }

  {
    /* Exercise both pointers pointing to the same object plus constant
       offset.  */
    char a2[2];               // { dg-message "at offset 1 into destination object 'a2' of size 2" "note" }
    char *p1 = a2 + 1;
    char *p2 = a2 + 2;

    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);         // { dg-warning "writing 2 bytes into a region of size 1 " }
  }

  {
    /* Exercise both pointers pointing to the same object plus offset
       in a known range.  */
    char a3[3];               // { dg-message "at offset \\\[1, 3] into destination object 'a3'" "note" }
    char *pi = a3 + i1;
    char *pj = a3 + i2;

    char *q = MIN (pi, pj);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);         // { dg-warning "writing 3 bytes into a region of size 2 " }
  }

  {
    /* Exercise both pointers pointing to the same object plus variable
       offset.  Verify that no offset is mentioned in the note (since
       its unknown, printing its full range is unnecessary).  */
    char a4[4];               // { dg-message ": destination object 'a4'" "note" }
    char *pi = a4 + vi;
    char *pj = a4 + vi;

    char *q = MIN (pi, pj);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 4);
    memset (q, 0, 5);         // { dg-warning "writing 5 bytes into a region of size 4 " }
  }

  {
    /* Exercise a pointer pointing to a known object with one pointing
       to an unknown object.  */
    char a5[5];               // { dg-message ": destination object 'a5'" "note" }
    char *p = ptr;
    char *q = MIN (p, a5);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 5);
    memset (q, 0, 6);         // { dg-warning "writing 6 bytes into a region of size 5 " }
  }

  {
    /* Exercise a pointer pointing to a known object plus constant offset
       with one pointing to an unknown object.  */
    char a6[6];               // { dg-message "(at offset 1 into )?destination object 'a6'" "note" }
    char *p1 = ptr;
    char *p2 = a6 + 1;
    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 6);
    memset (q, 0, 7);         // { dg-warning "writing 7 bytes into a region of size 6 " }
  }

  {
    /* Exercise a pointer pointing to a known object with one pointing
       to an unknown object plus constant offset.  */
    char a7[7];               // { dg-message ": destination object 'a7'" "note" }
    char *p1 = a7;
    char *p2 = ptr + 1;
    /* Since p1 points to a7[0] it must be less than any pointer to a7
       plus positive offset, and so Q == P1.  */
    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 7);
    memset (q, 0, 8);         // { dg-warning "writing 8 bytes into a region of size 7 " }
  }

  {
    /* Exercise a pointer pointing to a known object plus constant offset
       with one pointing to an unknown object plus a different constant
       offset.  */
    char a8[8];               // { dg-message "at offset 1 into destination object 'a8'" "note" }
    char *p1 = a8 + 1;
    char *p2 = ptr + 2;
    /* Since P1 points to A8[1] it must be less than or equal to any
       pointer to A8 plus positive offset.  Either way, Q must point
       to A8[1].  */
    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 7);
    memset (q, 0, 8);         // { dg-warning "writing 8 bytes into a region of size 7 " }
  }

  {
    /* Same as above but with larger offsets.  */
    char a9[9];               // { dg-message "at offset 3 into destination object 'a9'" "note" }
    char *p1 = a9 + 3;
    char *p2 = ptr + 4;
    /* Since P1 points to A9[3] it must be less than or equal to any
       pointer anywhere into A9 plus 4, so Q must point to A9[3].  */
    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 6);
    memset (q, 0, 7);         // { dg-warning "writing 7 bytes into a region of size 6 " }
  }

  {
    /* Same as above but with the offsets reversed.  */
    char a10[10];              // { dg-message "at offset 5 into destination object 'a10'" "note" }
    char *p1 = a10 + 10;
    char *p2 = ptr + 5;
    /* Since P1 points just past the end of A10 it could be either less
       or equal to another pointer anywhere into A10 plus 3 because
       the other pointer itself could start at a non-zero offset that's
       not reflected in the determined offset).  */
    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 5);
    memset (q, 0, 6);         // { dg-warning "writing 6 bytes into a region of size 5 " }
  }

  {
    char a3[3];               // { dg-message ": destination object 'a3'" "note" }
    char *p1 = ptr;
    char *p2 = a3 + i1;
    char *q = MIN (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 4);         // { dg-warning "writing 4 bytes into a region of size 3 " }
  }
}


void test_max (void)
{
  const int i1 = SR (1, INT_MAX);
  const int i2 = SR (2, INT_MAX);

  {
    /* Exercise both pointers pointing to the same object plus constant
       offset.  */
    char a2[2];               // { dg-message "at offset 1 into destination object 'a2' of size 2" "note" }
    char *pi = a2 + 1;
    char *pj = a2 + 2;

    char *q = MAX (pi, pj);

    memset (q, 0, 1);
    memset (q, 0, 2);         // { dg-warning "writing 2 bytes into a region of size 1 " }
  }

  {
    /* Exercise both pointers pointing to the same object plus offset
       in a known range.  */
    char a3[3];               // { dg-message "at offset \\\[1, 3] into destination object 'a3'" "note" }
    char *pi = a3 + i1;
    char *pj = a3 + i2;

    char *q = MAX (pi, pj);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);         // { dg-warning "writing 3 bytes into a region of size 2 " }
  }

  {
    /* Exercise both pointers pointing to the same object plus variable
       offset.  Verify that no offset is mentioned in the note (since
       its unknown, printing its full range is unnecessary).  */
    char a4[4];               // { dg-message ": destination object 'a4'" "note" }
    char *pi = a4 + vi;
    char *pj = a4 + vi;

    char *q = MAX (pi, pj);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 3);
    memset (q, 0, 4);
    memset (q, 0, 5);         // { dg-warning "writing 5 bytes into a region of size 4 " }
  }

  {
    /* Exercise a pointer pointing to a known object with one pointing
       to an unknown object.  */
    char a5[5];               // { dg-message ": destination object 'a5'" "note" }
    char *p = ptr;
    char *q = MAX (p, a5);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 5);
    memset (q, 0, 6);         // { dg-warning "writing 6 bytes into a region of size 5 " }
  }

  {
    /* Exercise a pointer pointing to a known object plus constant offset
       with one pointing to an unknown object.  */
    char a6[6];               // { dg-message "at offset 1 into destination object 'a6'" "note" }
    char *p1 = ptr;
    char *p2 = a6 + 1;
    char *q = MAX (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 5);
    memset (q, 0, 6);         // { dg-warning "writing 6 bytes into a region of size 5 " }
    memset (q, 0, 7);         // { dg-warning "writing 7 bytes into a region of size 5 " }
  }

  {
    /* Exercise a pointer pointing to a known object with one pointing
       to an unknown object plus constant offset.  */
    char a7[7];               // { dg-message "at offset 1 into destination object 'a7'" "note" }
    char *p1 = a7;
    char *p2 = ptr + 1;
    /* Since p1 points to a7[0] it must be less than any pointer to a7
       plus positive offset, and so Q == P2.  */
    char *q = MAX (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 6);
    memset (q, 0, 7);         // { dg-warning "writing 7 bytes into a region of size 6 " }
    memset (q, 0, 8);         // { dg-warning "writing 8 bytes into a region of size 6 " }
  }

  {
    /* Exercise a pointer pointing to a known object plus constant offset
       with one pointing to an unknown object plus a different constant
       offset.  */
    char a8[8];               // { dg-message "at offset 2 into destination object 'a8'" "note" }
    char *p1 = a8 + 1;
    char *p2 = ptr + 2;
    /* Since P1 points to A8[1] it must be less than or equal to any
       pointer to A8 plus positive offset.  Either way, Q must point
       to A8[2].  */
    char *q = MAX (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 6);
    memset (q, 0, 7);         // { dg-warning "writing 7 bytes into a region of size 6 " }
    memset (q, 0, 8);         // { dg-warning "writing 8 bytes into a region of size 6 " }
  }

  {
    /* Same as above but with larger offsets.  */
    char a9[9];               // { dg-message "at offset 4 into destination object 'a9'" "note" }
    char *p1 = a9 + 3;
    char *p2 = ptr + 4;
    /* Since P1 points to A9[3] it must be less than or equal to any
       pointer anywhere into A9 plus 4, so Q must point to A9[4].  */
    char *q = MAX (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 5);
    memset (q, 0, 6);         // { dg-warning "writing 6 bytes into a region of size 5 " }
  }

  {
    /* Same as above but with the offsets reversed.  */
    char a10[10];              // { dg-message "at offset 10 into destination object 'a10'" "note" }
    char *p1 = a10 + 10;
    char *p2 = ptr + 5;
    /* Since P1 points just past the end of A10 it could be either less
       or equal to another pointer anywhere into A10 plus 3 because
       the other pointer itself could start at a non-zero offset that's
       not reflected in the determaxed offset).  */
    char *q = MAX (p1, p2);

    memset (q, 0, 1);         // { dg-warning "writing 1 byte into a region of size 0 " }
  }

  {
    char a11[11];             // { dg-message "at offset \\\[1, 11] into destination object 'a11'" "note" }
    char *p1 = ptr;
    char *p2 = a11 + i1;
    char *q = MAX (p1, p2);

    memset (q, 0, 1);
    memset (q, 0, 2);
    memset (q, 0, 10);
    memset (q, 0, 11);        // { dg-warning "writing 11 bytes into a region of size 10 " }
  }
}

